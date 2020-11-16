setClass(Class = "Model.PremSolver", contains = "IModel")

Model.PremSolver <- function(args = ArgSet.PremSolver(), id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.PremSolver", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.PremSolver", "IPlan"),
   definition = function(object, var, result = list()) {
      if (length(GetPremTable(var)) > 1) {
         riskClasses <- names(GetPremTable(var))
      } else {
         riskClasses <- NA_character_
      }
      unitFace <- GetArgValue(object, "UnitFaceAmt")
      issDate <- GetArgValue(object, "ProjStartDate")
      premMode <- GetArgValue(object, "PricPremMode")
      # In R 4.0.0, there appear to be a bug when using makeCluster in MacOS.  A way to work around is to add argument setup_strategy = "sequential".
      # This issue does not exist when running R 4.0.0 in Windows 10.
      cl <- parallel::makeCluster(min(parallel::detectCores() - 1, length(riskClasses)), setup_strategy = "sequential")
      for(pkg in (.packages())) {
         eval(parse(text = paste0("parallel::clusterEvalQ(cl, expr = library(", pkg, "))")))
      }
      parallel::clusterExport(cl, "object", envir = environment())
      result <- parallel::parLapply(
         cl,
         X = as.list(riskClasses),
         fun = function(rc, model, plan) {
            # Create a new premium table
            ageRange <- GetPricIssAge(GetArgs(model), rc)
            tbl <- Table.IA(minAge = min(ageRange), maxAge = max(ageRange), tBase = unitFace)
            if (is.na(rc)) {
               tblId <- plan@PremTable
            } else {
               tblId <- plan@PremTable[rc]
            }
            SetId(tbl) <- ifelse(startsWith(tblId, "Prem."), tblId, paste0("Prem.", tblId))
            # Calculate premium rate for each issue age
            calcResult <- lapply(
               X = as.list(ageRange[1]:ageRange[2]),
               FUN = function(age) {
                  cov <- Cov(
                     planId = GetPlanId(plan),
                     issDate = issDate,
                     issAge = as.integer(age),
                     riskClass = rc,
                     faceAmt = GetPricFaceAmt(GetArgs(model), rc, age),
                     premMode = premMode,
                     modPrem = 1     # Set initial value
                  )
                  profitMargin <- GetTargProfitMargin(GetArgs(model), rc, age)
                  tmpResult <- optimize(f = .CalcSolverObjective,
                                        interval = GetArgValue(model, "Interval"),
                                        cov, plan, unitFace, profitMargin, GetArgs(model),
                                        tol = GetArgValue(model, "Tolerance")
                  )
                  premRate <- round(tmpResult$minimum, digits = GetArgValue(model, "Digits"))
                  return(c(age, premRate))
               }
            )
            for (rate in calcResult) {
               tbl@TValue[as.character(rate[1]), 1] <- rate[2]
            }
            eval(expr = parse(text = paste0("result$", GetId(tbl), " <- tbl")))
         },
         object, var
      )
      parallel::stopCluster(cl)
      return(result)
   }
)

.CalcSolverObjective <- function(premRate, cov, plan, unitFace, profitMargin, args) {
   SetModPrem(cov) <- premRate * GetFaceAmt(cov) / unitFace * GetModFactor(plan, GetPremMode(cov)) + GetPolFee(plan, GetPremMode(cov))
   model <- Model.DCF(args)
   tmp <- Run(model, cov, result = list())
   discrepancy <- abs(tmp$PV$Total.Net / tmp$PV$Prem - profitMargin)
   return(discrepancy)
}



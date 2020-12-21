#' @include IPlan.IAJS.R
NULL

setClass(
   Class = "Model.PremSolver.IA.JS",
   contains = "Model.PremSolver"
)

Model.PremSolver.IA.JS <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(
      Class = "Model.PremSolver.IA.JS",
      Args = args,
      Descrip = as.character(descrip)
   )
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.PremSolver.IA.JS", "IPlan.IA.JS"),
   definition = function(object, var, result = list()) {
      if (length(GetPremTable(var)) > 1) {
         riskClasses <- names(GetPremTable(var))
      } else {
         riskClasses <- NA_character_
      }
      unitFace <- GetArgValue(object, "UnitFaceAmt")
      issDate <- GetArgValue(object, "ProjStartDate")
      premMode <- GetArgValue(object, "PricPremMode")
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
                  SetIssAge2(cov) <- GetIssAge2(model, cov)
                  SetRiskClass2(cov) <- GetRiskClass2(model, cov)
                  profitMargin <- GetTargProfitMargin(GetArgs(model), rc, age)
                  tmpResult <- optimize(f = .CalcSolverObjective.IAJS,
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

.CalcSolverObjective.IAJS <- function(premRate, cov, plan, unitFace, profitMargin, args) {
   SetModPrem(cov) <- premRate * GetFaceAmt(cov) / unitFace * GetModFactor(plan, GetPremMode(cov)) + GetPolFee(plan, GetPremMode(cov))
   model <- Model.DCF(args)
   tmp <- Run(model, cov, result = list())
   discrepancy <- abs(tmp$PV$Total.Net / tmp$PV$Prem - profitMargin)
   return(discrepancy)
}

setMethod(
   f = GetIssAge2,
   signature = "Model.PremSolver.IA.JS",
   definition = function(object, cov) {
      arg <- GetArgValue(object, "SurvrAgeDiff")
      issAge2 <- arg[GetRiskClass(cov)] + GetIssAge(cov)
      return(issAge2)
   }
)

setMethod(
   f = GetRiskClass2,
   signature = "Model.PremSolver.IA.JS",
   definition = function(object, cov) {
      arg <- GetArgValue(object, "SurvrRiskClass")
      return(arg[GetRiskClass(cov)])
   }
)

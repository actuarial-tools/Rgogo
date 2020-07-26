#' @include IPlan.LT.R
NULL

setClass(
   Class = "IPlan.End",
   contains = "IPlan.LT",
   slots = c(
      CVTable = "character"
   )
)

setValidity(
   Class = "IPlan.End",
   method = function(object) {
      err <- New.SysMessage()
      # Validate cash value table
      v <- Validator.Names(hasNames = (length(object@CVTable) > 1))
      if (Validate(v, object@CVTable) != TRUE) {
         AddMessage(err) <- "Invalid cash value table setting."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.End <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                      premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                      polFee = numeric(0), premTaxRate = numeric(0L), cvTable = character(0L),
                      commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                      rein = character(0L), planId = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.End",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               CVTable = cvTable,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               PremTaxRate = premTaxRate,
               Rein = rein,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(planId)
   return(plan)
}

setMethod(
   f = "GetCVTable",
   signature = "IPlan.End",
   definition = function(object, cov) {
      if (length(object@CVTable) == 0) {
         return(NULL)
      }
      if (length(object@CVTable) == 1) {
         tblId <- object@CVTable
      } else {
         tblId <- object@CVTable[GetRiskClass(object, cov)]
      }
      tblId <- ifelse(startsWith(tblId, "CV."), tblId, paste0("CV.", tblId))
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "SetCVTable<-",
   signature = "IPlan.End",
   definition = function(object, value) {
      object@CVTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCVRateVector",
   signature = "IPlan.End",
   definition = function(object, cov) {
      covMonths <- GetCovMonths(object, cov)
      covYears <- ceiling(covMonths / 12)
      cvTable <- GetCVTable(object, cov)
      if (is.null(cvTable)) {return(rep(0, covMonths + 1))}
      cvFactor <- LookUp(cvTable, cov)[1:covYears]     # Cash value factors at the end of policy years
      cv1 <- matrix(data = cvFactor, nrow = covYears)     # Cash value factors at the end of policy years, in 1 by N matrix
      cv0 <- matrix(data = c(0, cvFactor)[1:covYears], nrow = covYears)     # Cash value factors at the beginning of policy years, in 1 by N matrix
      s <- matrix(data = seq(from = 1/12, to = 1, length.out = 12), ncol = 12)
      m <- cv0 %*% (1-s) + cv1 %*% s
      cvRate <- c(0, as.vector(t(m))[1:covMonths])
      return(cvRate)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      matBen <- c(rep(0, covMonths), GetFaceAmt(cov))
      resultContainer %<>% AddProjection(projItem = "Ben.Mat", projValue = matBen)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjCV",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer){
      projCV <- GetFaceAmt(cov) * GetCVRateVector(object, cov)
      resultContainer %<>% AddProjection(projItem = "CV", projValue = projCV)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      cv <- resultContainer$Proj$CV
      resultContainer %<>% AddProjection(projItem = "Ben.Sur", projValue = cv)
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      return(resultContainer)
   }
)




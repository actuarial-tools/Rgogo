setClass(
   Class = "IPlan.Fund",
   contains = c("IPlan", "VIRTUAL"),
   slots = c(
      CovPeriod = "numeric",
      PremPeriod = "numeric"
   )
)

setMethod(
   f = "GetCovYears",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@CovPeriod["CovYears"]), Inf, object@CovPeriod["CovYears"])
      years2 <- ifelse(is.na(object@CovPeriod["CovToAge"]), Inf, object@CovPeriod["CovToAge"] - GetIssAge(cov))
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetPremYears",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@PremPeriod["PremYears"]), Inf, object@PremPeriod["PremYears"])
      years2 <- ifelse(is.na(object@PremPeriod["PremToAge"]), Inf, object@PremPeriod["PremToAge"] - GetIssAge(cov))
      premYears <- min(years1, years2)
      return(premYears)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      return(GetRiskClass(cov))
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      modPrem <- GetModPrem(cov)
      if (HasValue(modPrem)) {
         prem <- FillZeroIfNA(rep(c(modPrem, rep(0, times = (12 / GetPremMode(cov) - 1))), length.out = GetPremMonths(object, cov)), len = covMonths)
      } else {
         prem <- rep(0, length.out = covMonths)
      }
      resultContainer$Proj$Prem <- prem
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      resultContainer$Proj$Comm <- rep(0, length.out = covMonths)
      resultContainer$Proj$Comm.Ovrd <- rep(0, length.out = covMonths)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      resultContainer$Proj$Ben.Dth <- resultContainer$Proj$Fund
      return(resultContainer)
   }
)

setMethod(
   f = "GetSurChrg",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      return(rep(0, GetCovMonths(object, cov)))
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      resultContainer$Proj$Ben.Sur <- resultContainer$Proj$Fund - GetSurChrg(object, cov, resultContainer)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      resultContainer$Proj$Ben.Mat <- c(rep(0, length.out = covMonths - 1), resultContainer$Proj$Fund[covMonths])
      return(resultContainer)
   }
)

setMethod(
   f = "ProjFund",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      stop("'ProjFund' method must be implemented by a class implementing 'IPlan.Fund'")
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer = list()) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjFund(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      return(resultContainer)
   }
)






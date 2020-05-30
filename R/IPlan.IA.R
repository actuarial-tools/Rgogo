# Immediate Annuity
setClass(
   Class = "IPlan.IA",
   contains = "IPlan.End",
   slots = c(
      AnuMode = "integer",
      AnuTiming = "integer",
      CrtnMonths = "integer"
   )
)



setValidity(
   Class = "IPlan.IA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate annuity mode @AnuMode
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.InList(valuesAllowed = c(1L, 2L, 4L, 12L))
         ),
         object@AnuMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of annuity mode is invalid.  It must be 1L (annual), 2L (semiannual), 4L (quarterly) or 12L (monthly)."
      }
      # Validate annuity timing @AnuTiming
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.InList(valuesAllowed = c(0L, 1L))
         ),
         object@AnuTiming
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of annuity timing is invalid.  It must be 0L (beginning of policy month) or 1L (end of policy month)."
      }
      # Validate guarantee period @CrtnMonths
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.Range(minValue = 0)
         ),
         object@CrtnMonths
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of guarantee period (in number of months) is invalid.  It must be a non-negative integer."
      }
      return(ifelse(NoMessage(err), TRUE, GetMessage(object)))
   }
)


IPlan.IA <- function(planId = character(), anuYears = NA, anuToAge = NA, anuMode = 12L, anuTiming = 0L, crtnMonths = 0L) {
   stopifnot(any(!is.na(c(anuYears, anuToAge))))
   covPeriod <- c(CovYears = anuYears, CovToAge = as.integer(anuToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = 1/12)
   plan <- new(Class = "IPlan.IA",
               Id = as.character(planId),
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               AnuMode = as.integer(anuMode),
               AnuTiming = as.integer(anuTiming),
               CrtnMonths = as.integer(crtnMonths)
   )
   return(plan)
}


setMethod(
   f = "GetRiskClass",
   signature = "IPlan.IA",
   definition = function (object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "GetCovYears",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "GetPremYears",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "GetPremTable",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetPremTable<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetPremRate",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "GetModFactor",
   signature = "IPlan.IA",
   definition = function(object, premMode) {
      return(1)
   }
)


setMethod(
   f = "SetModFactor<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      stop("Method 'SetModFactor<-' cannot be invoked by a class of or extending 'IPlan.IA'.")
   }
)


setMethod(
   f = "GetPolFee",
   signature = "IPlan.IA",
   definition = function(object, premMode) {
      if (length(object@PolFee) == 0) {
         return(0)
      } else {
         return(object@PolFee)
      }
   }
)


setMethod(
   f = "SetPolFee<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetCommSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetCommSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetPremTaxRate",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetPremTaxRate<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetRein",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      stop("The method 'GetRein' cannot be invoked by a class of or extending 'IPlan.IA'.")
   }
)


setMethod(
   f = "SetRein<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      stop("The method 'SetRein<-' cannot be invoked by a class of or extending 'IPlan.IA'.")
   }
)


setMethod(
   f = "GetAnuMode",
   signature = "IPlan.IA",
   definition = function(object) {
      return(object@AnuMode)
   }
)


setMethod(
   f = "SetAnuMode<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@AnuMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetAnuTiming",
   signature = "IPlan.IA",
   definition = function(object) {
      return(object@AnuTiming)
   }
)


setMethod(
   f = "SetAnuTiming<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@AnuTiming <- as.integer(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetAnuCrtnMonths",
   signature = "IPlan.IA",
   definition = function(object) {
      return(object@CrtnMonths)
   }
)


setMethod(
   f = "SetAnuCrtnMonths<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@CrtnMonths <- as.integer(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "ProjPrem",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # If cov contains modal premium information (i.e. cov@ModPrem contains a value), use that information to preject premium;
      # otherwise, look up premium table to calculate the modal premium.
      singlePrem <- GetModPrem(cov)
      if (!HasValue(singlePrem)) {
         premRate <- GetPremRate(object, cov)
         stopifnot(!is.na(premRate))
         singlePrem <- premRate[1] * GetFaceAmt(cov) + GetPolFee(object, premMode)
      }
      prem <- c(singlePrem, rep(0, length.out = GetCovMonths(object, cov)))
      premTax <- prem * GetPremTaxRate(object, cov)
      if (!all(prem == 0)) {
         resultContainer$Proj.Prem <- prem
         resultContainer %<>% AddProjection(projItem = "Prem", projValue = prem)
      }
      if (!all(premTax == 0)) {
         resultContainer$Proj.Prem.Tax <- premTax
         resultContainer %<>% AddProjection(projItem = "Prem.Tax", projValue = premTax)
      }
      return(resultContainer)
   }
)


setMethod(
   f = "ProjComm",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      callNextMethod()
   }
)


setMethod(
   f = "ProjDthBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # No death benefit.
      return(resultContainer)
   }
)


setMethod(
   f = "ProjRein",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      stop("The method 'ProjRein' cannot be invoked by a class of or extending 'IPlan.IA'.")
   }
)


setMethod(
   f = "GetCVTable",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "SetCVTable<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      callNextMethod()
   }
)


setMethod(
   f = "GetCVRateVector",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      callNextMethod()
   }
)


setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      # a <- rep(cov@AnuBen, covMonths)
      a <- rep(GetFaceAmt(cov) / object@AnuMode, length.out = covMonths)   # Face amount of coverage contrains annualized annuity benefit information.
      m <- (seq(from = 1, to = length(a)) - 1) %% (12 / object@AnuMode) == 0
      if (object@AnuTiming == 0) v <- c(a * m, 0) else v <- c(0, a * m)
      resultContainer$Proj.Ben.Anu <- v
      resultContainer %<>% AddProjection(projItem = "Ben.Anu", projValue = v)
      return(resultContainer)
   }
)


setMethod(
   f = "ProjMatBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # No maturity benefit
      return(resultContainer)
   }
)


setMethod(
   f = "ProjCV",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer){
      projCV <- GetFaceAmt(cov) * GetCVRateVector(object, cov)
      resultContainer$Proj.CV <- projCV
      resultContainer %<>% AddProjection(projItem = "CV", projValue = projCV)
      return(resultContainer)
   }
)


setMethod(
   f = "ProjSurBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      projCV <- resultContainer$Proj.CV
      resultContainer$Proj.Ben.Sur <- projCV
      resultContainer %<>% AddProjection(projItem = "Ben.Sur", projValue = projCV)
      return(resultContainer)
   }
)


setMethod(
   f = "Project",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      return(resultContainer)
   }
)




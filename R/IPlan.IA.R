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
      # Validate @Rein: no reinsurance
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 0),
         object@Rein
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Reinsurance is not permitted for an object of 'IPlan.IA' class."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.IA <- function(anuYears = NA, anuToAge = NA,
                     anuMode = 12L, anuTiming = 0L, crtnMonths = 0L,
                     premTable = character(0L), polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(anuYears, anuToAge))))
   covPeriod <- c(CovYears = anuYears, CovToAge = as.integer(anuToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = 1/12)
   plan <- new(Class = "IPlan.IA",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               CrtnMonths = crtnMonths,
               PremTable = premTable,
               ModFactor = c("1" = 1),
               PolFee = polFee,
               CVTable = character(0L),
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               PremTaxRate = premTaxRate,
               Rein = character(0L),
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "SetModFactor<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      stop("Method 'SetModFactor<-' cannot be invoked by an object of 'IPlan.IA' class.")
   }
)

setMethod(
   f = "GetRein",
   signature = "IPlan.IA",
   definition = function(object) {
      return(object@Rein)
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
      # Annuity due (payable at the beginning of period): object@AnuMode == 0L
      # Annuity immediate (payable at the end of period): object@AnuMode == 1L
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
      # cov@PremMode and object@ModFactor are ignored.
      singlePrem <- GetModPrem(cov)
      if (!HasValue(singlePrem)) {
         premRate <- GetPremRate(object, cov)
         stopifnot(!is.na(premRate))
         singlePrem <- premRate[1] * GetFaceAmt(cov) + GetPolFee(object, premMode)
      }
      prem <- c(singlePrem, rep(0, length.out = GetCovMonths(object, cov) - 1))
      premTax <- prem * GetPremTaxRate(object, cov)
      if (!all(prem == 0)) {
         resultContainer %<>% AddProjection(projItem = "Prem", projValue = prem)
      }
      if (!all(premTax == 0)) {
         resultContainer %<>% AddProjection(projItem = "Prem.Tax", projValue = premTax)
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # No death benefit
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
   f = "ProjRein",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # No reinsurance
      return(resultContainer)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      a <- rep(GetFaceAmt(cov) / object@AnuMode, length.out = covMonths)   # Face amount of coverage contrains annualized annuity benefit information.
      m <- (seq(from = 1, to = length(a)) - 1) %% (12 / object@AnuMode) == 0
      # if (object@AnuTiming == 0) v <- c(a * m, 0) else v <- c(0, a * m)
      resultContainer %<>% AddProjection(projItem = "Ben.Anu", projValue = a * m)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjCV",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer){
      # No cash value
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # No surrender
      return(resultContainer)
   }
)


setMethod(
   f = "Project",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      return(resultContainer)
   }
)




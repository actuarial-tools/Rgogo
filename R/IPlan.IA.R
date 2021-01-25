setClass(Class = "IPlan.IA", contains = "IPlan.Anu")

setValidity(
   Class = "IPlan.IA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @PremPeriod
      isValid <- object@PremPeriod == c(PremYears = 1/12)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid premium period.  This shall be a single premium plan."
      }
      # Validate @AnuStart
      isValid <- object@AnuStart == c(AnuStartYear = 1)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid annuitization period.  This shall be an immediate annuity plan."
      }
      # Validate @ModFactor: Cannot set modal factor because this is a single premium product.
      isValid <- length(object@ModFactor) == 0
      if (isValid != TRUE) {
         AddMessage(err) <- "Cannot set modal factor because this is a single premium product."
      }
      # Validate @PolFee: must be a scalar.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0),
            Validator.Names(hasNames = FALSE)
         ),
         object@PolFee
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Policy fee must be a non-negative scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.IA <- function(anuYears = NA, anuToAge = NA,
                     anuMode = 12L, anuTiming = 0L, crtnMonths = 0L, anuBenSchd = numeric(0L),
                     premTable = character(0L), polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     id = character(0L), descrip = character(0L)) {
   # Define end of annuity payout period, which is also end of coverage period
   covPeriod <- c(CovYears = anuYears, CovToAge = anuToAge)
   covPeriod <- covPeriod[!is.na(covPeriod)]
   stopifnot(length(covPeriod) > 0)
   plan <- new(Class = "IPlan.Anu",
               CovPeriod = covPeriod,
               PremPeriod = c(PremYears = 1/12),
               AnuStart = c(AnuStartYear = 1),
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               CrtnMonths = crtnMonths,
               AnuBenSchd = anuBenSchd,
               PremTable = premTable,
               ModFactor = numeric(0L),
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CVTable = character(0L),
               SurChrgSchd = numeric(0L),
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               Rein = character(0L),
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetModFactor",
   signature = "IPlan.IA",
   definition = function(object, ...) {
      return(object@ModFactor)
   }
)

setMethod(
   f = "GetPolFee",
   signature = "IPlan.IA",
   definition = function(object, ...) {
      return(object@PolFee)
   }
)

setMethod(
   f = "GetModPrem",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      premRate <- GetPremRate(object, cov)
      stopifnot(!is.na(premRate))
      modPrem <- premRate[1] * GetFaceAmt(cov) + GetPolFee(object)
      return(modPrem)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # If cov contains modal premium information (i.e. cov@ModPrem contains a value), use that information to preject premium;
      # otherwise, look up premium table to calculate the modal premium.
      modPrem <- GetModPrem(cov)
      if (!HasValue(modPrem)) {
         modPrem <- GetModPrem(object, cov)
      }
      prem <- FillTail(modPrem, filler = 0, len = GetCovMonths(object, cov))
      premTax <- prem * GetPremTaxRate(object, cov)
      if (!all(prem == 0)) {
         resultContainer$Proj$Prem <- prem
      }
      if (!all(premTax == 0)) {
         resultContainer$Proj$Prem.Tax <- premTax
      }
      return(resultContainer)
   }
)



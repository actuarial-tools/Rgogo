# Joint and Survivor Immediate Annuity
setClass(
   Class = "IPlan.IA.JS",
   contains = "IPlan.IA",
   slots = c(
      SurvrBenPct = "numeric"
   )
)

setValidity(
   Class = "IPlan.IA.JS",
   method = function(object) {
      err <- New.SysMessage()
      # Validate annuity mode @SurvrBenPct
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.Range(minValue = 0)
         ),
         object@SurvrBenPct
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid survivor benefit percentage.  The value must be between 0 and 1."
      }
      # Validate certain period.  Must be zero.
      isValid <- (object@CrtnMonths == 0)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid annuity certain period.  It must be zero."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.IA.JS <- function(anuYears = NA, anuToAge = NA,
                        anuMode = 12L, anuTiming = 0L, anuAdjIndex = 0, survrBenPct = 0,
                        premTable = character(0L), polFee = numeric(0), premTaxRate = numeric(0L),
                        commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                        id = character(0L), descrip = character(0L)) {
   # Define premium period
   premPeriod <- c(PremYears = 1 / 12)
   # Define end of annuity payout period, which is the end of coverage period.
   covPeriod <- c(CovYears = anuYears, CovToAge = anuToAge)
   covPeriod <- covPeriod[!is.na(covPeriod)]
   plan <- new(Class = "IPlan.IA.JS",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               AnuAdjIndex = anuAdjIndex,
               CrtnMonths = 0L,
               SurvrBenPct = survrBenPct,
               PremTable = premTable,
               ModFactor = numeric(0L),
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CVTable = character(0L),
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
   f = "GetCovYears",
   signature = "IPlan.IA.JS",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@CovPeriod["CovYears"]), Inf, object@CovPeriod["CovYears"])
      issAge <- min(GetIssAge(cov), GetIssAge2(cov))
      years2 <- ifelse(is.na(object@CovPeriod["CovToAge"]), Inf, object@CovPeriod["CovToAge"] - issAge)
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetSurvrBenPct",
   signature = "IPlan.IA.JS",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@SurvrBenPct)
      } else {
         if (HasValue(GetFaceAmt2(cov))) {
            return(GetFaceAmt2(cov) / GetFaceAmt(cov))
         } else {
            return(object@SurvrBenPct)
         }
      }
   }
)

setMethod(
   f = "SetSurvrBenPct<-",
   signature = "IPlan.IA.JS",
   definition = function(object, value) {
      object@SurvrBenPct <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.IA.JS",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      resultContainer$Proj$Ben.Anu.Survr <- resultContainer$Proj$Ben.Anu * GetSurvrBenPct(object, cov)
      return(resultContainer)
   }
)



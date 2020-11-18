# Joint and Survivor Immediate Annuity
setClass(
   Class = "IPlan.IAJS",
   contains = "IPlan.IA",
   slots = c(
      SurvrBenPct = "numeric"
   )
)

setValidity(
   Class = "IPlan.IAJS",
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

IPlan.IAJS <- function(anuYears = NA, anuToAge = NA,
                     anuMode = 12L, anuTiming = 0L, survrBenPct = 0L,
                     premTable = character(0L), polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(anuYears, anuToAge))))
   stopifnot(length(polFee) <= 1)
   anuPeriod <- c(AnuYears = anuYears, AnuToAge = as.integer(anuToAge))
   anuPeriod <- anuPeriod[!is.na(anuPeriod)]
   plan <- new(Class = "IPlan.IAJS",
               AnuPeriod = anuPeriod,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               CrtnMonths = 0L,
               SurvrBenPct = survrBenPct,
               PremTable = premTable,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetCovYears",
   signature = "IPlan.IAJS",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@AnuPeriod["AnuYears"]), Inf, object@AnuPeriod["AnuYears"])
      if (GetLifeStatus(cov) == 1L & GetLifeStatus2(cov) == 1L) {
         issAge <- min(GetIssAge(cov), GetIssAge2(cov))
      } else if (GetLifeStatus(cov) == 1L & GetLifeStatus2(cov) == 0L) {
         issAge <- GetIssAge(cov)
      } else if (GetLifeStatus(cov) == 0L & GetLifeStatus2(cov) == 1L) {
         issAge <- GetIssAge2(cov)
      } else {
         stop("Both lives are dead!")
      }
      years2 <- ifelse(is.na(object@AnuPeriod["AnuToAge"]), Inf, object@AnuPeriod["AnuToAge"] - issAge)
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetSurvrBenPct",
   signature = "IPlan.IAJS",
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
   signature = "IPlan.IAJS",
   definition = function(object, value) {
      object@SurvrBenPct <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.IAJS",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      resultContainer$Proj$Ben.Anu.Survr <- resultContainer$Proj$Ben.Anu * GetSurvrBenPct(object, cov)
      return(resultContainer)
   }
)



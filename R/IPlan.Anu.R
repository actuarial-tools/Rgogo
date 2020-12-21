#' @include IPlan.End.R
NULL

setClass(
   Class = "IPlan.Anu",
   contains = "IPlan.End",
   slots = c(
      AnuStart = "numeric",
      AnuMode = "integer",
      AnuTiming = "integer",
      CrtnMonths = "integer",
      SurChrgSchd = "numeric"
   )
)

setValidity(
   Class = "IPlan.Anu",
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
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.Anu <- function(premYears = NA, premToAge = NA, anuStartYear = NA, anuStartAge = NA,
                      anuYears = NA, anuToAge = NA, anuMode = 12L, anuTiming = 0L, crtnMonths = 0L,
                      premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                      polFee = numeric(0), premTaxRate = numeric(0L),
                      cvTable = character(0L), surChrgSchd = numeric(0L),
                      commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                      id = character(0L), descrip = character(0L)) {
   # Define premium period
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   stopifnot(length(premPeriod) == 1)
   # Define beginning of annuity payout period
   anuStart <- c(AnuStartYear = anuStartYear, AnuStartAge = anuStartAge)
   anuStart <- anuStart[!is.na(anuStart)]
   stopifnot(length(anuStart) == 1)
   # Define end of annuity payout period, which is also end of coverage period
   covPeriod <- c(CovYears = anuYears, CovToAge = anuToAge)
   covPeriod <- covPeriod[!is.na(covPeriod)]
   stopifnot(length(covPeriod) >= 1)
   plan <- new(Class = "IPlan.Anu",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               AnuStart = anuStart,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               CrtnMonths = crtnMonths,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CVTable = cvTable,
               SurChrgSchd = surChrgSchd,
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
   f = "GetAnutzPeriod",
   signature = "IPlan.Anu",
   definition = function(object, cov) {
      if (!is.na(object@AnuStart["AnuStartYear"])) {
         anuStartMonth <- (object@AnuStart["AnuStartYear"] - 1) * 12 + 1
      } else {
         anuStartMonth <- (object@AnuStart["AnuStartAge"] - GetIssAge(cov)) * 12 + 1
      }
      anuEndMonth <- GetCovMonths(object, cov)
      return(anuStartMonth:anuEndMonth)
   }
)

setMethod(
   f = "GetAnuMode",
   signature = "IPlan.Anu",
   definition = function(object) {
      # Annuity due (payable at the beginning of period): object@AnuMode == 0L
      # Annuity immediate (payable at the end of period): object@AnuMode == 1L
      return(object@AnuMode)
   }
)

setMethod(
   f = "SetAnuMode<-",
   signature = "IPlan.Anu",
   definition = function(object, value) {
      object@AnuMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAnuTiming",
   signature = "IPlan.Anu",
   definition = function(object) {
      return(object@AnuTiming)
   }
)

setMethod(
   f = "SetAnuTiming<-",
   signature = "IPlan.Anu",
   definition = function(object, value) {
      object@AnuTiming <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAnuCrtnMonths",
   signature = "IPlan.Anu",
   definition = function(object) {
      return(object@CrtnMonths)
   }
)

setMethod(
   f = "SetAnuCrtnMonths<-",
   signature = "IPlan.Anu",
   definition = function(object, value) {
      object@CrtnMonths <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetSurChrgSchd",
   signature = "IPlan.Anu",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@SurChrgSchd)
      }
      if (HasValue(object@SurChrgSchd)) {
         schd <- FillZeroIfNA(rep(object@SurChrgSchd, each = 12), GetCovMonths(object, cov))
      } else {
         schd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(schd)
   }
)

setMethod(
   f = "SetSurChrgSchd<-",
   signature = "IPlan.Anu",
   definition = function(object, value) {
      object@SurChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRein",
   signature = "IPlan.Anu",
   definition = function(object, cov = NULL) {
      return(NULL)
   }
)

setMethod(
   f = "SetRein<-",
   signature = "IPlan.Anu",
   definition = function(object, value) {
      stop("Cannot set reinsurance for an instance of 'IPlan.Anu'.")
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      # No death benefit is implemented.  It needs to be implemented by users if there is any.
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      # No maturity benefit is implemented.  It needs to be implemented by users if there is any.
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurChrg",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      resultContainer$Proj$Chrg.Sur <- resultContainer$Proj$CV * GetSurChrgSchd(object, cov)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      resultContainer <- ProjSurChrg(object, cov, resultContainer)
      surBen <- resultContainer$Proj$CV - resultContainer$Proj$Chrg.Sur
      # Cannot surrender during annuitization period.
      anutzPeriod <- GetAnutzPeriod(object, cov)
      resultContainer$Proj$Ben.Sur <- surBen * !(seq_along(surBen) %in% anutzPeriod)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      anuPeriod <- GetAnutzPeriod(object, cov)
      anuMode <- GetAnuMode(object)
      a <- rep(GetFaceAmt(cov) / anuMode, length.out = length(anuPeriod))   # Face amount of coverage contrains annualized annuity benefit information.
      if (GetAnuTiming(object) == 0) {
         m <- (seq(from = 1, to = length(a)) - 1) %% (12 / anuMode) == 0
      } else {
         m <- (seq(from = 1, to = length(a))) %% (12 / anuMode) == 0
      }
      resultContainer$Proj$Ben.Anu <-  c(rep(0, length.out = anuPeriod[1] - 1), a * m)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjRein",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      # Reinsurance, if there is any, needs to be implemented by users.
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.Anu",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      return(resultContainer)
   }
)




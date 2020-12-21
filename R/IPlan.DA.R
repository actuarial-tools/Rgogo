#' @include IPlan.IA.R
NULL

setClass(
   Class = "IPlan.DA",
   contains = "IPlan.IA",
   slots = c(
      AccumPeriod = "numeric"
   )
)

setValidity(
   Class = "IPlan.DA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @AccumPeriod
      vg <- ValidatorGroup(
         Validator.Length(minLen = 1, maxLen = 2),
         Validator.Range(minValue = 0, maxValue = 120, allowNA = FALSE),
         Validator.Names(hasNames = TRUE, namesAllowed = c("AccumYears", "AccumToAge"))
      )
      if (Validate(vg, object@AccumPeriod) != TRUE) {
         AddMessage(err) <- "Invalid accumulation period setting."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.DA <- function(accumYears = NA, accumToAge = NA, premYears = NA, premToAge = NA,
                     anuYears = NA, anuToAge = NA, anuMode = 12L, anuTiming = 0L, crtnMonths = 0L, anuAdjIndex = 0,
                     premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                     polFee = numeric(0), premTaxRate = numeric(0L), cvTable = character(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     id = character(0L), descrip = character(0L)) {

   stopifnot(any(!is.na(c(accumYears, accumToAge))))
   stopifnot(any(!is.na(c(anuYears, anuToAge))))
   # Determine accumulation period
   accumPeriod <- c(AccumYears = accumYears, AccumToAge = as.integer(accumToAge))
   accumPeriod <- accumPeriod[!is.na(accumPeriod)]
   # Determine premium period.  If not specified, premium period is the same as accumulation period.
   if (all(is.na(c(premYears, premToAge)))) {
      premPeriod <- c(PremYears = accumYears, PremToAge = as.integer(accumToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   # Determine coverage period
   covPeriod <- c(CovYears = anuYears, CovToAge = anuToAge)
   covPeriod <- covPeriod[!is.na(covPeriod)]
   plan <- new(Class = "IPlan.DA",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               AccumPeriod = accumPeriod,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               AnuAdjIndex = anuAdjIndex,
               CrtnMonths = crtnMonths,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CVTable = cvTable,
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
   f = "GetAccumPeriod",
   signature = "IPlan.DA",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@AccumPeriod["AccumYears"]), Inf, object@AccumPeriod["AccumYears"])
      years2 <- ifelse(is.na(object@AccumPeriod["AccumToAge"]), Inf, object@AccumPeriod["AccumToAge"] - GetIssAge(cov))
      accumYears <- min(years1, years2)
      return(1:as.integer(accumYears * 12))

   }
)

setMethod(
   f = "GetAnutzPeriod",
   signature = "IPlan.DA",
   definition = function(object, cov) {
      accumPeriod <- GetAccumPeriod(object, cov)
      anutzPeriodStart <- accumPeriod[length(accumPeriod)] + 1
      anutzPeriodEnd <- GetCovMonths(object, cov)
      return(anutzPeriodStart:anutzPeriodEnd)
   }
)

setMethod(
   f = "GetCVTable",
   signature = "IPlan.DA",
   definition = function(object, cov) {
      return(GetCVTable(as(object, "IPlan.End"), cov))
   }
)

setMethod(
   f = "SetCVTable<-",
   signature = "IPlan.DA",
   definition = function(object, value) {
      object@CVTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetModFactor",
   signature = "IPlan.DA",
   definition = function(object, premMode) {
      return(GetModFactor(as(object, "IPlan.End"), cov))
   }
)

setMethod(
   f = "SetModFactor<-",
   signature = "IPlan.DA",
   definition = function(object, value) {
      object@ModFactor <- value
      validObject(object)
      return(object)
   }
)

# setMethod(
#    f = "ProjAnuBen",
#    signature = "IPlan.DA",
#    definition = function(object, cov, resultContainer) {
#       anuPeriod <- GetAnutzPeriod(object, cov)
#       anuMonths <- length(anuPeriod)
#       anuMode <- GetAnuMode(object)
#       cola <- c(0, GetAnuAdjIndex(object, cov))
#       cola <- rep(cola, each = 12, length.out = anuMonths) * (((1:anuMonths) - 1) %% 12 == 0)
#       cola <- cumprod(1 + cola)
#       a <- rep(GetFaceAmt(cov) / anuMode, length.out = anuMonths) * cola   # Face amount of coverage contrains annualized annuity benefit information.
#       if (GetAnuTiming(object) == 0) {
#          m <- (seq(from = 1, to = length(a)) - 1) %% (12 / anuMode) == 0
#       } else {
#          m <- (seq(from = 1, to = length(a))) %% (12 / anuMode) == 0
#       }
#       resultContainer$Proj$Ben.Anu <-  c(rep(0, length.out = anuPeriod[1] - 1), a * m)
#       return(resultContainer)
#    }
# )

setMethod(
   f = "Project",
   signature = "IPlan.DA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      return(resultContainer)
   }
)





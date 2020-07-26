#' @include IPlan.End.R
NULL

setClass(
   Class = "IPUA",
   contains = "IPlan.End",
   slots = c(
      PUASchd = "numeric",
      PUACreditMonth = "integer",
      PUAPaidOnDth = "logical",
      PUAPaidOnMat = "logical",
      PUAPaidOnSur = "logical"
   )
)

setValidity(
   Class = "IPUA",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "PUA.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'PUA.'"
         }
      }
      # '@PUACreditMonth must be between 1 and 12
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 1, maxValue = 12)
         ), object@PUACreditMonth
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUACreditMonth' must contain an integer value between 1 and 12."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@PUAPaidOnDth)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUAPaidOnDth' must contain a logical value (TRUE or FALSE)."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@PUAPaidOnMat)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUAPaidOnMat' must contain a logical value (TRUE or FALSE)."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@PUAPaidOnSur)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUAPaidOnSur' must contain a logical value (TRUE or FALSE)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPUA <- function(covYears = NA, covToAge = NA,
                 puaCreditMonth = 1L, puaPaidOnDth = TRUE, puaPaidOnMat = TRUE, puaPaidOnSur = TRUE,
                 puaId = character(0L), descrip = character(0L)) {
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = 0)
   pua <- new(
      Class = "IPUA",
      CovPeriod = covPeriod,
      PremPeriod = premPeriod,
      PUACreditMonth = puaCreditMonth,
      PUAPaidOnDth = puaPaidOnDth,
      PUAPaidOnMat = puaPaidOnMat,
      PUAPaidOnSur = puaPaidOnSur,
      Descrip = as.character(descrip)
   )
   SetPUAId(pua) <- as.character(puaId)
}

setMethod(
   f = "GetPUAId",
   signature = "IPUA",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetPUAId<-",
   signature = c("IPUA", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "PUA.")) {
         value <- paste0("PUA.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetCovYears",
   signature = "IPUA",
   definition = function(object, cov) {
      callNextMethod()
   }
)

setMethod(
   f = "GetPremYears",
   signature = "IPUA",
   definition = function(object, cov) {
      return(0)
   }
)

setMethod(
   f = "GetPremTable",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NULL)
   }
)

setMethod(
   f = "SetPremTable<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetPremTable<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetPremRate",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NA)
   }
)

setMethod(
   f = "GetModFactor",
   signature = "IPUA",
   definition = function(object, premMode) {
      return(NA)
   }
)

setMethod(
   f = "SetModFactor<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetModFactor<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetPolFee",
   signature = "IPUA",
   definition = function(object, premMode) {
      return(NA)
   }
)

setMethod(
   f = "SetPolFee<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetPolFee<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NA)
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetCommSchd<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NA)
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetOvrdOnCommSchd<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NA)
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetOvrdOnPremSchd<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetPremTaxRate",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NA)
   }
)

setMethod(
   f = "SetPremTaxRate<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetPremTaxRate<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "GetRein",
   signature = "IPUA",
   definition = function(object, cov) {
      return(NULL)
   }
)

setMethod(
   f = "SetRein<-",
   signature = "IPUA",
   definition = function(object, value) {
      stop("Method 'SetRein<-' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "SetPUASchd<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUASchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPUACreditMonth<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUACreditMonth <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "IsPUAPaidOnDth",
   signature = "IPUA",
   definition = function(object) {
      return(object@PUAPaidOnDth)
   }
)

setMethod(
   f = "IsPUAPaidOnMat",
   signature = "IPUA",
   definition = function(object) {
      return(object@PUAPaidOnMat)
   }
)

setMethod(
   f = "IsPUAPaidOnSur",
   signature = "IPUA",
   definition = function(object) {
      return(object@PUAPaidOnSur)
   }
)

setMethod(
   f = "IsPUAPaidOnDth<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUAPaidOnDth <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "IsPUAPaidOnMat<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUAPaidOnMat <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "IsPUAPaidOnSur<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUAPaidOnSur <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjPrem' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjComm' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "ProjPUA",
   signature = "IPUA",
   definition = function(object, cov, resultContainer, projStartDate = NULL) {
      if (is.null(projStartDate)) {
         projStartDate <- GetIssDate(cov)
      }
      covMonths <- GetCovMonths(cov)
      timeLine <- GetIssDate(cov) %m+% months(0:covMonths)
      puaSchd <- object@PUASchd[as.character(lubridate::year(timeLine))] * (timeLine >= projStartDate)
      puaRate <- ifelse(is.na(puaSchd), 0, puaSchd) *
         (lubridate::month(timeLine) == object@PUACreditMonth) *
         ((lubridate::interval(GetIssDate(cov), timeLine) / lubridate::years(1)) >= 1)      # Eligible for bonus only after completion of one full policy year.
      projPUA <- cumprod(1 + puaRate) * (GetFaceAmt(cov) + GetPUAAmt(cov)) - GetFaceAmt(cov)
      projPUA <- ifelse(timeLine < projStartDate, NA, projPUA)
      resultContainer %<>% AddProjection(projItem = "PUA", projValue = projPUA)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      if (IsPUAPaidOnDth(object)) {
         pua <- resultContainer$Proj$PUA
         resultContainer %<>% AddProjection(projItem = "Ben.Dth.PUA", projValue = pua)
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      if (IsPUAPaidOnMat(object)) {
         pua <- resultContainer$Proj$PUA
         covMonths <- GetCovMonths(GetPlan(cov), cov)
         matBen <- c(rep(0, covMonths), resultContainer$Proj$PUA[covMonths + 1])
         resultContainer %<>% AddProjection(projItem = "Ben.Mat.PUA", projValue = matBen)
      }
      return(resultContainer)
   }
)

setMethod(
   f = "GetCVRateVector",
   signature = "IPUA",
   definition = function(object, cov) {
      callNextMethod()
   }
)

setMethod(
   f = "ProjCV",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      projPUACV <- GetCVRateVector(object, cov) * resultContainer$Proj$PUA
      resultContainer %<>% AddProjection(projItem = "CV.PUA", projValue = projPUACV)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      if (IsPUAPaidOnSur(object)) {
         projPUACV <- resultContainer$Proj$CV.PUA
         resultContainer %<>% AddProjection(projItem = "Ben.Sur.PUA", projValue = projPUACV)
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjRein",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjRein' cannot be invoked by a class of or extending 'IPlan.UPA' class.")
   }
)

setMethod(
   f = "Project",
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- ProjPUA(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      return(resultContainer)
   }
)






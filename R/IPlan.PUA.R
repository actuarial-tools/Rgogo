#' @include IPlan.End.R
NULL

setClass(
   Class = "IPlan.PUA",
   contains = "IPlan.End",
   slots = c(
      PUASchd = "numeric",
      PUACredMonth = "integer",
      HasDthBen = "logical",
      HasMatBen = "logical",
      HasSurBen = "logical"
   )
)

setValidity(
   Class = "IPlan.PUA",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "PUA.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'PUA.'"
         }
      }
      # '@PUACredMonth must be between 1 and 12
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 1, maxValue = 12)
         ), object@PUACredMonth
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUACredMonth' must contain an integer value between 1 and 12."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@HasDthBen)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@HasDthBen' must contain a logical value (TRUE or FALSE)."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@HasMatBen)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@HasMatBen' must contain a logical value (TRUE or FALSE)."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@HasSurBen)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@HasSurBen' must contain a logical value (TRUE or FALSE)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.PUA <- function(covYears = NA, covToAge = NA,
                 puaSchd = numeric(0L), puaCredMonth = 1L, cvTable = character(0L),
                 hasDthBen = TRUE, hasMatBen = TRUE, hasSurBen = TRUE,
                 id = character(0L), descrip = character(0L)) {
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = 0)
   pua <- new(
      Class = "IPlan.PUA",
      CovPeriod = covPeriod,
      PremPeriod = premPeriod,
      PUASchd = puaSchd,
      PUACredMonth = puaCredMonth,
      CVTable = cvTable,
      HasDthBen = hasDthBen,
      HasMatBen = hasMatBen,
      HasSurBen = hasSurBen,
      Descrip = as.character(descrip)
   )
   SetPlanId(pua) <- id
   return(pua)
}

setMethod(
   f = "SetPlanId<-",
   signature = c("IPlan.PUA", "character"),
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
   f = "GetPremYears",
   signature = "IPlan.PUA",
   definition = function(object, cov) {
      return(0)
   }
)

setMethod(
   f = "SetPremTable<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetPremTable<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetModFactor<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetModFactor<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetPolFee<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetPolFee<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetCommSchd<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetOvrdOnCommSchd<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetOvrdOnPremSchd<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetPremTaxRate<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetPremTaxRate<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "SetRein<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      stop("Method 'SetRein<-' cannot be invoked by a class of or extending 'IPlan.PUA' class.")
   }
)

setMethod(
   f = "GetPUASchd",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@PUASchd)
      } else {
         covMonths <- GetCovMonths(object, cov)
         if (length(object@PUASchd) == 0) {
            return(rep(0, length.out = covMonths))
         } else {
            timeLine <- GetIssDate(cov) %m+% months(0:(covMonths - 1))
            puaSchd <- FillZeroIfNA(object@PUASchd[as.character(lubridate::year(timeLine))]) *
               (lubridate::month(timeLine) == GetPUACredMonth(object)) *
               # Eligible for bonus only after completion of one full policy year.
               ((lubridate::interval(GetIssDate(cov), timeLine) / lubridate::years(1)) >= 1)
            return(puaSchd)
         }
      }
   }
)

setMethod(
   f = "SetPUASchd<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      object@PUASchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPUACredMonth",
   definition = function(object) {
      return(object@PUACredMonth)
   }
)

setMethod(
   f = "SetPUACredMonth<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      object@PUACredMonth <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasDthBen",
   signature = "IPlan.PUA",
   definition = function(object) {
      return(object@HasDthBen)
   }
)

setMethod(
   f = "HasMatBen",
   signature = "IPlan.PUA",
   definition = function(object) {
      return(object@HasMatBen)
   }
)

setMethod(
   f = "HasSurBen",
   signature = "IPlan.PUA",
   definition = function(object) {
      return(object@HasSurBen)
   }
)

setMethod(
   f = "HasDthBen<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      object@HasDthBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasMatBen<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      object@HasMatBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasSurBen<-",
   signature = "IPlan.PUA",
   definition = function(object, value) {
      object@HasSurBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      resultContainer$Prem.Tax <- resultContainer$Prem <- rep(0, length.out = GetCovMonths(object, cov))
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      resultContainer$Comm <- resultContainer$Comm.Ovrd <- rep(0, length.out = GetCovMonths(object, cov))
      return(resultContainer)
   }
)

setMethod(
   f = "ProjPUA",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      if (is.null(resultContainer$.ArgSet)) {
         projStartDate <- GetIssDate(cov)
      } else {
         projStartDate <- GetProjStartDate(resultContainer$.ArgSet)
      }
      puaRate <- GetPUASchd(object, cov)
      # covMonths <- GetCovMonths(cov)
      # timeLine <- GetIssDate(cov) %m+% months(0:covMonths)
      # puaSchd <- object@PUASchd[as.character(lubridate::year(timeLine))] * (timeLine >= projStartDate)
      # puaRate <- ifelse(is.na(puaSchd), 0, puaSchd) *
      #    (lubridate::month(timeLine) == object@PUACredMonth) *
      #    ((lubridate::interval(GetIssDate(cov), timeLine) / lubridate::years(1)) >= 1)      # Eligible for bonus only after completion of one full policy year.
      projPUA <- cumprod(1 + puaRate) * (GetFaceAmt(cov) + GetPUAAmt(cov)) - GetFaceAmt(cov)
      projPUA <- ifelse(timeLine < projStartDate, NA, projPUA)
      resultContainer$Proj$PUA <- projPUA
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      if (HasDthBen(object)) {
         pua <- resultContainer$Proj$PUA
         resultContainer$Proj$Ben.Dth.PUA <- pua
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      if (HasMatBen(object)) {
         pua <- resultContainer$Proj$PUA
         covMonths <- GetCovMonths(object, cov)
         matBen <- c(rep(0, covMonths), resultContainer$Proj$PUA[covMonths + 1])
         resultContainer$Proj$Ben.Mat.PUA <- matBen
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjCV",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      projPUACV <- GetCVRateVector(object, cov) * resultContainer$Proj$PUA
      resultContainer$Proj$CV.PUA <- projPUACV
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      if (HasSurBen(object)) {
         projPUACV <- resultContainer$Proj$CV.PUA
         resultContainer$Proj$Ben.Sur.PUA <- projPUACV
      }
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.PUA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPUA(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      return(resultContainer)
   }
)






setClass(
   Class = "IPUA",
   #contains = "IPlan.End",
   contains = "IObject",
   slots = c(
      CovPeriod = "numeric",
      PUASchd = "numeric",
      PUACredMonth = "integer",
      CVTable = "character",
      HasDthBen = "logical",
      HasMatBen = "logical",
      HasSurBen = "logical"
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

IPUA <- function(covYears = NA, covToAge = NA,
                 puaSchd = numeric(0L), puaCredMonth = 1L, cvTable = character(0L),
                 hasDthBen = TRUE, hasMatBen = TRUE, hasSurBen = TRUE,
                 id = character(0L), descrip = character(0L)) {
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   pua <- new(
      Class = "IPUA",
      CovPeriod = covPeriod,
      PUASchd = puaSchd,
      PUACredMonth = puaCredMonth,
      CVTable = cvTable,
      HasDthBen = hasDthBen,
      HasMatBen = hasMatBen,
      HasSurBen = hasSurBen,
      Descrip = as.character(descrip)
   )
   SetId(pua) <- id
   return(pua)
}

setMethod(
   f = "SetId<-",
   signature = c("IPUA", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      object@Id <- ifelse (startsWith(value, "PUA."), value, paste0("PUA.", value))
      return(object)
   }
)

setMethod(
   f = "GetCovYears",
   signature = "IPUA",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@CovPeriod["CovYears"]), Inf, object@CovPeriod["CovYears"])
      years2 <- ifelse(is.na(object@CovPeriod["CovToAge"]), Inf, object@CovPeriod["CovToAge"] - GetIssAge(cov))
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetCovMonths",
   signature = "IPUA",
   definition = function(object, cov) {
      return(round(GetCovYears(object, cov) * 12, digits = 0))
   }
)

setMethod(
   f = "GetCVTable",
   signature = "IPUA",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CVTable)
      }
      if (length(object@CVTable) == 0) {
         return(NULL)
      }
      if (length(object@CVTable) == 1) {
         tblId <- object@CVTable
      } else {
         tblId <- object@CVTable[GetRiskClass(object, cov)]
      }
      tblId <- ifelse(startsWith(tblId, "CV."), tblId, paste0("CV.", tblId))
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "SetCVTable<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@CVTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCVRateVector",
   signature = "IPUA",
   definition = function(object, cov) {
      covMonths <- GetCovMonths(object, cov)
      covYears <- ceiling(covMonths / 12)
      cvTable <- GetCVTable(object, cov)
      if (is.null(cvTable)) {return(rep(0, covMonths))}
      cvFactor <- LookUp(cvTable, cov)[1:covYears]     # Cash value factors at the end of policy years
      cv1 <- matrix(data = cvFactor, nrow = covYears)     # Cash value factors at the end of policy years, in 1 by N matrix
      cv0 <- matrix(data = c(0, cvFactor)[1:covYears], nrow = covYears)     # Cash value factors at the beginning of policy years, in 1 by N matrix
      s <- matrix(data = seq(from = 1/12, to = 1, length.out = 12), ncol = 12)
      m <- cv0 %*% (1-s) + cv1 %*% s
      cvRate <- as.vector(t(m))[1:covMonths]
      return(cvRate)
   }
)

setMethod(
   f = "GetPUASchd",
   signature = "IPUA",
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
   signature = "IPUA",
   definition = function(object, value) {
      object@PUASchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPUACredMonth",
   signature = "IPUA",
   definition = function(object) {
      return(object@PUACredMonth)
   }
)

setMethod(
   f = "SetPUACredMonth<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@PUACredMonth <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasDthBen",
   signature = "IPUA",
   definition = function(object) {
      return(object@HasDthBen)
   }
)

setMethod(
   f = "HasMatBen",
   signature = "IPUA",
   definition = function(object) {
      return(object@HasMatBen)
   }
)

setMethod(
   f = "HasSurBen",
   signature = "IPUA",
   definition = function(object) {
      return(object@HasSurBen)
   }
)

setMethod(
   f = "HasDthBen<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@HasDthBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasMatBen<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@HasMatBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "HasSurBen<-",
   signature = "IPUA",
   definition = function(object, value) {
      object@HasSurBen <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPUA",
   signature = "IPUA",
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
   signature = "IPUA",
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
   signature = "IPUA",
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
   signature = "IPUA",
   definition = function(object, cov, resultContainer) {
      projPUACV <- GetCVRateVector(object, cov) * resultContainer$Proj$PUA
      resultContainer$Proj$CV.PUA <- projPUACV
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPUA",
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
   signature = "IPUA",
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






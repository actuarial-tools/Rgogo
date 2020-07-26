setClass(
   Class = "ArgSet.PPM",
   contains = "ArgSet.DCF",
   slots = c(
      ValuDate = "Date",
      ResFloor = "numeric"
   )
)

setValidity(
   Class = "ArgSet.PPM",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@ValuDate)
      if(isValid != TRUE) {
         AddMessage(err) <- "Value of slot '@ValuDate' must be of length 1."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@ResFloor)
      if(isValid != TRUE) {
         AddMessage(err) <- "Value of slot '@ResFloor' must be of length 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.PPM <- function(valuDate = as.Date("1899-12-31"),
                       mortAssump = character(),
                       lapseAssump = character(),
                       expnsAssump = character(),
                       intrAssump = character(),
                       applyMortMargin = TRUE,
                       applyLapseMargin = TRUE,
                       applyExpnsMargin = TRUE,
                       applyIntrMargin = TRUE,
                       reserveFloor = -Inf,
                       id = character(0L),
                       descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.PPM",
      ValuDate = lubridate::as_date(valuDate),
      ProjStartDate = lubridate::as_date(valuDate) + 1,
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyLapseMargin = as.logical(applyLapseMargin),
      ApplyExpnsMargin = as.logical(applyExpnsMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      ResFloor = as.numeric(reserveFloor),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "SetArgValue",
   signature = "ArgSet.PPM",
   definition = function(object, ...) {
      valueList <- list(...)
      argNames <- names(valueList)
      if ("ProjStartDate" %in% argNames) {
         stop("Setting argument value for slot '@ProjStartDate' of class 'ArgSet.PPM' is not permitted.")
      }
      for (i in 1:length(valueList)) {
         slot(object, argNames[i]) <- valueList[[i]]
         if (argNames[i] == "ValuDate") {
            slot(object, "ProjStartDate") <- valueList[[i]] + 1
         }
      }
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetValuDate",
   signature = "ArgSet.PPM",
   definition = function(object) {
      return(object@ValuDate)
   }
)

setMethod(
   f = "SetValuDate<-",
   signature = "ArgSet.PPM",
   definition = function(object, value) {
      object@ValuDate <- lubridate::as_date(value)
      object@ProjStartDate <- lubridate::as_date(value) + 1
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetProjStartDate<-",
   signature = "ArgSet.PPM",
   definition = function(object, value) {
      stop("Method 'SetProjStartDate<-' cannot be invoked for an object of class 'ArgSet.PPM'")
   }
)

setMethod(
   f = "GetResFloor",
   signature = "ArgSet.PPM",
   definition = function(object) {
      return(object@ResFloor)
   }
)

setMethod(
   f = "SetResFloor<-",
   signature = "ArgSet.PPM",
   definition = function(object, value) {
      object@ResFloor <- as.numeric(value)
      validObject(object)
      return(object)
   }
)


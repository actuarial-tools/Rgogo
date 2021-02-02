setClass(
   Class = "ArgSet.PremSolver",
   contains = "ArgSet.DCF",
   slots = c(
      PricIssAge = "integer_or_list",
      PricFaceAmt = "numeric_or_list",
      PricPremMode = "integer",
      UnitFaceAmt = "numeric",
      TargProfitMargin = "numeric_or_list",
      Interval = "numeric",
      Tolerance = "numeric",
      Digits = "integer"
   )
)

setValidity(
   Class = "ArgSet.PremSolver",
   method = function(object) {
      err <- New.SysMessage()
      # UnitFaceAmt
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = FALSE)
         ),
         object@UnitFaceAmt
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@UnitFaceAmt' of class 'ArgSet.PremSolver' must be a numeric vector of length 1."
      }
      # TargProfitMargin
      isValid <- Validate(
         ValidatorGroup(Validator.Length(minLen = 1, maxLen = 999999)),
         object@TargProfitMargin
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@TargProfitMargin' of class 'ArgSet.PremSolver' must be a numeric vector of length 1."
      }
      # Interval: must be a numeric vector of length 2.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 2, maxLen = 2),
            Validator.Range(allowNA = FALSE)
         ),
         object@Interval
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Interval' of class 'ArgSet.PremSolver' must be a numeric vector of length 2."
      }
      # Tolerance: must contain a numeric value.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(allowNA = FALSE)
         ),
         object@Tolerance
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Tolerance' of class 'ArgSet.PremSolver' must contain a numeric scalar."
      }
      # Digits: must contain an integer value.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(allowNA = FALSE)
         ),
         object@Digits
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Digits' of class 'ArgSet.PremSolver' must contain an integer scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.PremSolver <- function(projStartDate,
                              pricIssAge,
                              pricFaceAmt,
                              pricPremMode,
                              unitFaceAmt,
                              targProfitMargin,
                              mortAssump = character(0L),
                              lapseAssump = character(0L),
                              expnsAssump = character(0L),
                              intrAssump = character(0L),
                              applyMortMargin = FALSE,
                              applyLapseMargin = FALSE,
                              applyExpnsMargin = FALSE,
                              applyIntrMargin = FALSE,
                              digits = 2L,
                              interval = c(0, 99999),
                              tolerance = 10^-4,
                              id = character(0L),
                              descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.PremSolver",
      ProjStartDate = lubridate::as_date(projStartDate),
      PricIssAge = pricIssAge,
      PricFaceAmt = pricFaceAmt,
      PricPremMode = pricPremMode,
      UnitFaceAmt = unitFaceAmt,
      TargProfitMargin = targProfitMargin,
      Digits = digits,
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = applyMortMargin,
      ApplyLapseMargin = applyLapseMargin,
      ApplyExpnsMargin = applyExpnsMargin,
      ApplyIntrMargin = applyIntrMargin,
      Interval = c(min(interval), max(interval)),
      Tolerance = tolerance,
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "GetPricIssAge",
   signature = "ArgSet.PremSolver",
   definition = function(object, riskClass = NA_character_) {
      if (is.na(riskClass)) {
         return(object@PricIssAge)
      }
      if (is.list(object@PricIssAge)) {
         return(object@PricIssAge[[as.character(riskClass)]])
      } else {
         return(c(min(object@PricIssAge), max(object@PricIssAge)))
      }
   }
)

setMethod(
   f = "GetPricFaceAmt",
   signature = "ArgSet.PremSolver",
   definition = function(object, riskClass = NA_character_, issAge = NA_integer_) {
      if (is.na(riskClass) & is.na(issAge)) {
         return(object@PricFaceAmt)
      }
      argValue <- object@PricFaceAmt
      if (is.list(argValue)) {
         argValue <- argValue[[as.character(riskClass)]]
      }
      if (length(argValue) == 1) {
         return(argValue)
      } else {
         return(argValue[as.character(issAge)])
      }
   }
)

setMethod(
   f = "GetPricPremMode",
   signature = "ArgSet.PremSolver",
   definition = function(object) {
      return(object@PricPremMode)
   }
)

setMethod(
   f = "GetUnitFaceAmt",
   signature = "ArgSet.PremSolver",
   definition = function(object) {
      return(object@UnitFaceAmt)
   }
)

setMethod(
   f = "GetTargProfitMargin",
   signature = "ArgSet.PremSolver",
   definition = function(object, riskClass, issAge) {
      if (is.na(riskClass) & is.na(issAge)) {
         return(object@TargProfitMargin)
      }
      argValue <- object@TargProfitMargin
      if (is.list(argValue)) {
         argValue <- argValue[[as.character(riskClass)]]
      }
      if (length(argValue) == 1) {
         return(argValue)
      } else {
         return(argValue[as.character(issAge)])
      }
   }
)

setMethod(
   f = "GetRndDigits",
   signature = "ArgSet.PremSolver",
   definition = function(object) {
      return(object@Digits)
   }
)

setMethod(
   f = "GetOptmzInterval",
   signature = "ArgSet.PremSolver",
   definition = function(object) {
      return(object@Interval)
   }
)

setMethod(
   f = "GetOptmzTolerance",
   signature = "ArgSet.PremSolver",
   definition = function(object) {
      return(object@Tolerance)
   }
)

setMethod(
   f = "SetPricIssAge<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@PricIssAge <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPricFaceAmt<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@PricFaceAmt <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPricPremMode<-",
   signature = "ArgSet.PremSolver",
   definition = function(object,value) {
      object@PricPremMode <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetUnitFaceAmt<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@UnitFaceAmt <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetTargProfitMargin<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@TargProfitMargin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetRndDigits<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@Digits <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetOptmzInterval<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      stopifnot(length(value) == 2)
      object@Interval <- c(min(value), max(value))
      return(object)
   }
)

setMethod(
   f = "SetOptmzTolerance<-",
   signature = "ArgSet.PremSolver",
   definition = function(object, value) {
      object@Tolerance <- value
      validObject(object)
      return(object)
   }
)




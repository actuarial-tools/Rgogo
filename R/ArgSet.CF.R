#' @include IArgSet.R
#' @include IMortAssump.R
#' @include ILapseAssump.R
#' @include IExpnsAssump.R
NULL


setClass(
   Class = "ArgSet.CF",
   contains = "IArgSet",
   slots = c(
      ProjStartDate = "Date",
      MortAssump = "character_or_IMortAssump",
      LapseAssump = "character_or_ILapseAssump",
      ExpnsAssump = "character_or_IExpnsAssump",
      ApplyMortMargin = "logical",
      ApplyLapseMargin = "logical",
      ApplyExpnsMargin = "logical"
   )
)


setValidity(
   Class = "ArgSet.CF",
   method = function(object) {
      err <- New.SysMessage()
      # @ProjStartDate: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ProjStartDate)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ProjStartDate' must contain a value of length 1."
      }
      # @ProjStartDate: must be the first day of a calendar month
      if (lubridate::day(object@ProjStartDate) != 1) {
         AddMessage(err) <- "Cashflow projection starting date must be the first day of a calendar month."
      }
      # @MortAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@MortAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MortAssump' must contain a value of length not greater than 1."
      }
      # @LapseAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@LapseAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@LapseAssump' must contain a value of length not greater than 1."
      }
      # @ExpnsAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@ExpnsAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ExpnsAssump' must contain a value of length not greater than 1."
      }
      # @ApplyMortMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyMortMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyMortMargin' must contain a value of length 1."
      }
      # @ApplyLapseMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyLapseMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyLapseMargin' must contain a value of length 1."
      }
      # @ApplyExpnsMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyExpnsMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyExpnsMargin' must contain a value of length 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


ArgSet.CF <- function(...) {
   args <- new(
      Class = "ArgSet.CF",
      ProjStartDate = as.Date("1900-01-01"),
      ApplyMortMargin = FALSE,
      ApplyLapseMargin = FALSE,
      ApplyExpnsMargin = FALSE
   )
   if (length(list(...)) > 0) {
      args <- SetArgValue(args, ...)
   }
   return(args)
}


setMethod(
   f = "GetArgValue",
   signature = "ArgSet.CF",
   definition = function(object, argName) {
      value <- switch (argName,
                       MortAssump = GetMortAssump(object),
                       LapseAssump = GetLapseAssump(object),
                       ExpnsAssump = GetExpnsAssump(object),
                       callNextMethod()
      )
      return(value)
   }
)


setMethod(
   f = "GetMortAssump",
   signature = "ArgSet.CF",
   definition = function(object) {
      slotValue <- object@MortAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "MortAssump."), slotValue, paste0("MortAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(MortAssump())
         }
      } else {
         return(slotValue)
      }
   }
)


setMethod(
   f = "GetLapseAssump",
   signature = "ArgSet.CF",
   definition = function(object) {
      slotValue <- object@LapseAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "LapseAssump."), slotValue, paste0("LapseAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(LapseAssump())
         }
      } else {
         return(slotValue)
      }
   }
)


setMethod(
   f = "GetExpnsAssump",
   signature = "ArgSet.CF",
   definition = function(object) {
      slotValue <- object@ExpnsAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "ExpnsAssump."), slotValue, paste0("ExpnsAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(ExpnsAssump())
         }
      } else {
         return(slotValue)
      }
   }
)



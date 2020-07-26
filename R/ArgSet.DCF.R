#' @include IIntrAssump.R
NULL

setClass(
   Class = "ArgSet.DCF",
   contains = "ArgSet.CF",
   slots = c(
      IntrAssump = "character_or_IIntrAssump",
      ApplyIntrMargin = "logical"
   )
)

setValidity(
   Class = "ArgSet.DCF",
   method = function(object) {
      err <- New.SysMessage()
      # @IntrAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@IntrAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@IntrAssump' must contain a value of length not greater than 1."
      }
      # @ApplyIntrMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyIntrMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyIntrMargin' must contain a value of length 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.DCF <- function(projStartDate = "1900-01-01",
                       mortAssump = character(0L), lapseAssump = character(0L),
                       expnsAssump = character(0L), intrAssump = character(0L),
                       applyMortMargin = FALSE, applyLapseMargin = FALSE,
                       applyExpnsMargin = FALSE, applyIntrMargin = FALSE,
                       id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.DCF",
      ProjStartDate = lubridate::as_date(projStartDate),
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyLapseMargin = as.logical(applyLapseMargin),
      ApplyExpnsMargin = as.logical(applyExpnsMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "GetArgValue",
   signature = "ArgSet.DCF",
   function(object, argName) {
      value <- switch(argName,
                      IntrAssump = GetIntrAssump(object),
                      callNextMethod()
      )
      return(value)
   }
)

setMethod(
   f = "GetIntrAssump",
   signature = "ArgSet.DCF",
   definition = function(object) {
      slotValue <- object@IntrAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "IntrAssump."), slotValue, paste0("IntrAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(IntrAssump())
         }
      } else {
         return(slotValue)
      }
   }
)

setMethod(
   f = "SetIntrAssump<-",
   signature = "ArgSet.DCF",
   definition = function(object, value) {
      object@IntrAssump <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ApplyIntrMargin",
   signature = "ArgSet.DCF",
   definition = function(object) {
      if (object@ApplyIntrMargin == TRUE) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }
)

setMethod(
   f = "ApplyIntrMargin<-",
   signature = "ArgSet.DCF",
   definition = function(object, value) {
      object@ApplyIntrMargin <- as.logical(value)
      validObject(object)
      return(object)
   }
)

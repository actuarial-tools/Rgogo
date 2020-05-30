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



ArgSet.DCF <- function(...) {
   args <- new(
      Class = "ArgSet.DCF",
      ProjStartDate = as.Date("1900-01-01"),
      ApplyMortMargin = FALSE,
      ApplyLapseMargin = FALSE,
      ApplyExpnsMargin = FALSE,
      ApplyIntrMargin = FALSE
   )
   if (length(list(...)) > 0) {
      args <- SetArgValue(args, ...)
   }
   return(args)
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




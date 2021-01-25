
setClass(
   Class = "ArgSet.NP",
   contains = "IArgSet",
   slots = c(
      MortAssump = "character_or_IMortAssump",
      IntrAssump = "character_or_IIntrAssump",
      ApplyMortMargin = "logical",
      ApplyIntrMargin = "logical"
   )
)

setValidity(
   Class = "ArgSet.NP",
   method = function(object) {
      err <- New.SysMessage()
      # @MortAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@MortAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MortAssump' must contain a value of length not greater than 1."
      }
      # @IntrAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@IntrAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@IntrAssump' must contain a value of length not greater than 1."
      }
      # @ApplyMortMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyMortMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyMortMargin' must contain a value of length 1."
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

ArgSet.NP <- function(mortAssump = character(0L), intrAssump = character(0L),
                      applyMortMargin = FALSE, applyIntrMargin = FALSE,
                      id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.NP",
      MortAssump = mortAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "GetArgValue",
   signature = "ArgSet.NP",
   definition = function(object, argName) {
      value <- switch (argName,
                       MortAssump = GetMortAssump(object),
                       IntrAssump = GetIntrAssump(object),
                       callNextMethod()
      )
      return(value)
   }
)

setMethod(
   f = "GetMortAssump",
   signature = "ArgSet.NP",
   definition = function(object) {
      slotValue <- object@MortAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "MortAssump."), slotValue, paste0("MortAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(NULL)
         }
      } else {
         return(slotValue)
      }
   }
)

setMethod(
   f = "GetIntrAssump",
   signature = "ArgSet.NP",
   definition = function(object) {
      slotValue <- object@IntrAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "IntrAssump."), slotValue, paste0("IntrAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(NULL)
         }
      } else {
         return(slotValue)
      }
   }
)

setMethod(
   f = "SetMortAssump<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@MortAssump <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetIntrAssump<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@IntrAssump <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ApplyMortMargin",
   signature = "ArgSet.NP",
   definition = function(object) {
      if (object@ApplyMortMargin == TRUE) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }
)

setMethod(
   f = "ApplyIntrMargin",
   signature = "ArgSet.NP",
   definition = function(object) {
      if (object@ApplyIntrMargin == TRUE) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }
)

setMethod(
   f = "ApplyMortMargin<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@ApplyMortMargin <- as.logical(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ApplyIntrMargin<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@ApplyIntrMargin <- as.logical(value)
      validObject(object)
      return(object)
   }
)

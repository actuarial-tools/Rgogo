setClass(
   Class = "ArgSet.CF.Fund",
   contains = "ArgSet.CF",
   slots = c(
      PremAssump = "character_or_IPremAssump",
      IntrCredAssump = "character_or_IIntrCredAssump",
      ApplyPremMargin = "logical"
   )
)

setValidity(
   Class = "ArgSet.CF.Fund",
   method = function(object) {
      err <- New.SysMessage()
      # @PremAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@PremAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PremAssump' must contain a value of length not greater than 1."
      }
      # @IntrCredAssump: length of value must not be greater than 1.
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@IntrCredAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@IntrCredAssump' must contain a value of length not greater than 1."
      }
      # @ApplyPremMargin: length of value must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@ApplyPremMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@ApplyPremMargin' must contain a value of length 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.CF.Fund <- function(projStartDate = "1900-01-01",
                           mortAssump = character(0L),
                           lapseAssump = character(0L),
                           expnsAssump = character(0L),
                           premAssump = character(0L),
                           intrCredAssump = character(0L),
                           applyMortMargin = FALSE,
                           applyLapseMargin = FALSE,
                           applyExpnsMargin = FALSE,
                           applyPremMargin = FALSE,
                           id = character(0L),
                           descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.CF.Fund",
      ProjStartDate = lubridate::as_date(projStartDate),
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      PremAssump = premAssump,
      IntrCredAssump = intrCredAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyLapseMargin = as.logical(applyLapseMargin),
      ApplyExpnsMargin = as.logical(applyExpnsMargin),
      ApplyPremMargin = as.logical(applyPremMargin),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "GetPremAssump",
   signature = "ArgSet.CF.Fund",
   definition = function(object) {
      slotValue <- object@PremAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "PremAssump."), slotValue, paste0("PremAssump.", slotValue))
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
   f = "SetPremAssump<-",
   signature = "ArgSet.CF.Fund",
   definition = function(object, value) {
      object@PremAssump <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIntrCredAssump",
   signature = "ArgSet.CF.Fund",
   definition = function(object) {
      slotValue <- object@IntrCredAssump
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "IntrCredAssump."), slotValue, paste0("IntrCredAssump.", slotValue))
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
   f = "SetIntrCredAssump<-",
   signature = "ArgSet.CF.Fund",
   definition = function(object, value) {
      object@IntrCredAssump <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ApplyPremMargin",
   signature = "ArgSet.CF.Fund",
   definition = function(object) {
      return(object@ApplyPremMargin)
   }
)

setMethod(
   f = "ApplyPremMargin<-",
   signature = "ArgSet.CF.Fund",
   definition = function(object, value) {
      object@ApplyPremMargin <- value
      validObject(object)
      return(object)
   }
)




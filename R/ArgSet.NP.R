setClass(
   Class = "ArgSet.NP",
   contains = "ArgSet.DCF",
   slots = c(
      PremMode = "integer",
      DthTiming = "integer"
   )
)

setValidity(
   Class = "ArgSet.NP",
   method = function(object) {
      err <- New.SysMessage()
      # Validate slot @PremMode
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0L, maxLen = 1L),
            Validator.InList(c(1, 2, 4, 12))
         ),
         object@PremMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The value assigned to slot @PremMode is invalid."
      }
      # Validate slot @DthTiming
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, max = 1L),
            Validator.InList(c(0, 1))
         ),
         object@DthTiming
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The value assigned to slot @DthTiming is invalid."
      }
      # Lapse assumption is not permitted.
      isValid <- Validate(Validator.Length(minLen = 0, maxLen = 0), object@LapseAssump)
      if (isValid != TRUE) {
         AddMessage(err) <- "Lapse assumpption cannot be set for NLP model."
      }
      if (NoMessage(err)) return(TRUE) else return(GetMessage(err))
   }
)

ArgSet.NP <- function(mortAssump = character(0L),
                      intrAssump = character(0L),
                      applyMortMargin = TRUE,
                      applyIntrMargin = TRUE,
                      premMode = integer(0L),
                      dthTiming = 1L,
                      id = character(0L),
                      descrip = character(0L)) {
   args <- new(
      Class = "ArgSet.NP",
      ProjStartDate = lubridate::NA_Date_,
      MortAssump = mortAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      ApplyLapseMargin = FALSE,
      ApplyExpnsMargin = FALSE,
      PremMode = premMode,
      DthTiming = dthTiming,
      Descrip = as.character(descrip)
   )
   SetArgSetId(args) <- as.character(id)
   return(args)
}

setMethod(
   f = "GetPremMode",
   signature = "ArgSet.NP",
   definition = function(object) {
      return(object@PremMode)
   }
)

setMethod(
   f = "SetPremMode<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@PremMode <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetDthTiming",
   signature = "ArgSet.NP",
   definition = function(object) {
      return(object@DthTiming)
   }
)

setMethod(
   f = "SetDthTiming<-",
   signature = "ArgSet.NP",
   definition = function(object, value) {
      object@DthTiming <- value
      validObject(object)
      return(object)
   }
)


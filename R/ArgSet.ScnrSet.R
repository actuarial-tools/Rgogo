#' @include IModel.R
NULL


setClass(
   Class = "ArgSet.ScnrSet",
   contains = "IArgSet",
   slots = c(
      Model = "character_or_IModel",
      ScnrList = "list"     # A named list with name attribute representing scenario id.
   )
)


setValidity(
   Class = "ArgSet.ScnrSet",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@Model)
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value '@Model' must be one."
      }
      isValid <- Validate(Validator.Names(hasNames = TRUE), object@ScnrList)
      if (isValid != TRUE) {
         AddMessage(object) <- "Slot value '@ScnrList' must be a list with name attribute."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


ArgSet.ScnrSet <- function(model, scenarios) {
   object <- new(Class = "ArgSet.ScnrSet", Model = model, ScnrList = scenarios)
   return(object)
}





#' @include IModel.R
NULL

setClass(
   Class = "ArgSet.ScnrTest",
   contains = "IArgSet",
   slots = c(
      Model = "character_or_IModel",
      ScnrList = "list",
      TestItem = "character"
   )
)

setValidity(
   Class = "ArgSet.ScnrTest",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@Model)
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value '@Model' must be one."
      }
      isValid <- Validate(Validator.Names(hasNames = TRUE), object@ScnrList)
      if (isValid != TRUE) {
         AddMessage(object) <- "Slot value '@ScnrList' must be a list with name attribute representing scenario id."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.ScnrTest <- function(model, scnrList, testItem, id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.ScnrTest",
      Model = model,
      ScnrList = scnrList,
      TestItem = testItem,
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}



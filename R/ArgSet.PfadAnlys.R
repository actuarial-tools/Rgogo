#' @include IArgSet.R
#' @include IModel.R
NULL

setClass(
   Class = "ArgSet.PfadAnlys",
   contains = "IArgSet",
   slots = c(
      ValuModel = "character_or_IModel"
   )
)

setValidity(
   Class = "ArgSet.PfadAnlys",
   method = function(object) {
      err <- New.SysMessage()
      # Validate object@ValuModel
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@ValuModel)
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value '@ValuModel' must be 1."
      }
      return(ifelse(NoMessage(err), TRUE, GetMessage(err)))
   }
)

ArgSet.PfadAnlys <- function(valuModel, id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.PfadAnlys",
      ValuModel = valuModel,
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

setMethod(
   f = "GetModel",
   signature = "ArgSet.PfadAnlys",
   definition = function(object) {
      if (is.character(object@ValuModel)) {
         modelId <- ifelse(startsWith(object@ValuModel, "Model."), object@ValuModel, paste0("Model.", object@ValuModel))
         return(eval(expr = parse(text = modelId)))
      } else {
         return(object@ValuModel)
      }
   }
)



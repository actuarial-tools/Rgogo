#' @include IValidator.R
NULL


setClass(
   Class = "Validator.InList",
   contains = "IValidator",
   slots = c(ValueList = "list")
)


Validator.InList <- function(valuesAllowed) {
   if (!is.list(valuesAllowed)) {
      valuesAllowed <- as.list(valuesAllowed)
   }
   vldtr <- new(
      Class = "Validator.InList",
      ValueList = valuesAllowed
   )
}


setMethod(
   f = "Validate",
   signature = c("Validator.InList", "ANY"),
   definition = function(object, value) {
      return(all(value %in% object@ValueList))
   }
)



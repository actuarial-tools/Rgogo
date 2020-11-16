#' @include IValidator.R
NULL


setClass(
   Class = "Validator.InList",
   contains = "IValidator",
   slots = c(
      ValueList = "list",
      AllowNA = "logical"
   )
)


Validator.InList <- function(valuesAllowed, allowNA = FALSE) {
   if (!is.list(valuesAllowed)) {
      valuesAllowed <- as.list(valuesAllowed)
   }
   vldtr <- new(
      Class = "Validator.InList",
      ValueList = valuesAllowed,
      AllowNA = allowNA
   )
}


setMethod(
   f = "Validate",
   signature = c("Validator.InList", "ANY"),
   definition = function(object, value) {
      isValid <- value %in% object@ValueList
      if (object@AllowNA) {
         isValid <- isValid | is.na(value)
      }
      return(all(isValid))
   }
)



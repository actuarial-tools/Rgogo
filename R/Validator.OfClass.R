#' @include IValidator.R
NULL


setClass(
   Class = "Validator.OfClass",
   contains = "IValidator",
   slots = c(ClassName = "character")
)


setValidity(
   Class = "Validator.OfClass",
   method = function(object) {
      if (!identical(is.character(object@ClassName), TRUE)) {
         return("Slot '@ClassName' must be a character vector.")
      }
      return(TRUE)
   }
)


Validator.OfClass <- function(clsName) {
   vldtr <- new(Class = "Validator.OfClass", ClassName = clsName)
}


setMethod(
   f = "Validate",
   signature = c("Validator.OfClass", "character"),
   definition = function(object, value) {
      if (!is.list(value)) {
         value <- as.list(value)
      }
      isValid <- rep(FALSE, length.out = length(value))
      for (cls in object@ClassName) {
         isValid <- isValid | unlist(lapply(value, function(x) {return(is(x, cls))}))
      }
      return(all(isValid))
   }
)




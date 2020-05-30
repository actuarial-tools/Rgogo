#' @include IValidator.R
NULL


setClass(
   Class = "Validator.Length",
   contains = "IValidator",
   slots = c(
      MinLen = "numeric",
      MaxLen = "numeric"
   )
)


Validator.Length <- function(minLen, maxLen) {
   vldtr <- new(Class = "Validator.Length", MinLen = minLen, MaxLen = maxLen)
   return(vldtr)
}


setValidity(
   Class = "Validator.Length",
   method = function(object) {
      if (!identical(Is.WholeNumber(object@MinLen), TRUE)) {
         return("'@MinLen' must contain a non-negative integer.")
      }
      if (!identical(Is.WholeNumber(object@MaxLen), TRUE)) {
         return("'@MaxLen' must contain a non-negative integer.")
      }
      if (object@MinLen > object@MaxLen) {
         return("'@MaxLen' must not be smaller than '@MinLen'.")
      }
      return(TRUE)
   }
)


setMethod(
   f = "Validate",
   signature = c("Validator.Length", "ANY"),
   definition = function(object, value) {
      len <- length(value)
      isValid <- all(len >= object@MinLen, len <=object@MaxLen)
      return(isValid)
   }
)


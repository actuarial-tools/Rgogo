#' @include IValidator.R
NULL


setClass(
   Class = "Validator.Range",
   contains = "IValidator",
   slots = c(
      MinValue = "numeric",
      MaxValue = "numeric",
      AllowNA = "logical"
   )
)


Validator.Range <- function(minValue = -Inf, maxValue = Inf, allowNA = FALSE) {
   vldtr <- new(Class = "Validator.Range", MinValue = minValue, MaxValue = maxValue, AllowNA = allowNA)
   return(vldtr)
}


setValidity(
   Class = "Validator.Range",
   method = function(object) {
      if (!all(length(object@MinValue) == 1, !is.na(object@MinValue))) {
         return("Slot '@MinValue' must contain a numeric value of length 1.")
      }
      if (!all(length(object@MaxValue) == 1, !is.na(object@MaxValue))) {
         return("Slot '@MaxValue' must contain a numeric value of length 1.")
      }
      if (!identical(object@MinValue <= object@MaxValue, TRUE)) {
         return("'@MaxValue' must not be smaller than '@MinValue'.")
      }
      return(TRUE)
   }
)


setMethod(
   f = "Validate",
   signature = c("Validator.Range", "numeric"),
   definition = function(object, value) {
      if (identical(all(value >= object@MinValue, na.rm = object@AllowNA) & all(value <= object@MaxValue, na.rm = object@AllowNA), TRUE)) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }
)


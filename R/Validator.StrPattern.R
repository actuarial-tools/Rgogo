setClass(
   Class = "Validator.StrPattern",
   contains = "IValidator",
   slots = c(Pattern = "character")
)


setValidity(
   Class = "Validator.StrPattern",
   method = function(object) {
      if (length(object@Pattern) != 1) {
         return("Specification of pattern (slot '@Pattern') is invalid.")
      }
      return(TRUE)
   }
)


Validator.StrPattern <- function(pattern) {
   vldtr <- new(Class = "Validator.StrPattern", Pattern = pattern)
   return(vldtr)
}


setMethod(
   f = "Validate",
   signature = c("Validator.StrPattern", "character"),
   definition = function(object, value) {
      isValid <- all(grepl(object@Pattern, value))
      return(isValid)
   }
)




#' @include IValidator.R
NULL


setClass(
   Class = "ValidatorGroup",
   contains = "IValidator",
   slots = c(ValidatorList = "list")
)


ValidatorGroup <- function(...) {
   vldtr <- new(Class = "ValidatorGroup", ValidatorList = list(...))
   return(vldtr)
}


setValidity(
   Class = "ValidatorGroup",
   method = function(object) {
      isValid <- all(unlist(lapply(object@ValidatorList, function(vldtr) {return(is(vldtr, "IValidator"))})))
      return(isValid)
   }
)


setMethod(
   f = "Validate",
   signature = c("ValidatorGroup", "ANY"),
   definition = function(object, value) {
      isValid <- unlist(
         lapply(
            object@ValidatorList,
            function(vldtr) {
               return(Validate(vldtr, value))
            }
         )
      )
      return(all(isValid))
   }
)




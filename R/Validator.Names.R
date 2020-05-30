#' @include IValidator.R
NULL


setClass(
   Class = "Validator.Names",
   contains = "IValidator",
   slots = c(
      HasNames = "logical",
      NameList = "list"
   )
)


Validator.Names <- function(hasNames, namesAllowed = character(0L)) {
   if (!is.list(namesAllowed)) {
      namesAllowed <- as.list(namesAllowed)
   }
   vldtr <- new(
      Class = "Validator.Names",
      HasNames = hasNames,
      NameList = namesAllowed
   )
}


setMethod(
   f = "Validate",
   signature = c("Validator.Names", "ANY"),
   definition = function(object, value) {
      # Skip validation if value does not contain any element.
      if (length(value) == 0) {
         return(TRUE)
      }
      if (object@HasNames == TRUE) {             # - Shoud have name attribute.
         if (is.null(names(value))) {            # -- The value does not have name attribute.
            return(FALSE)
         } else {                                # -- The value has name attributes
            if (length(object@NameList) > 0) {   # --- allowed names are specified
               return(all(names(value) %in% object@NameList))
            } else {                             # --- no restrictions on the allowed names
               return(!is.null(names(value)))
            }
         }
      } else {                                   # - Should not have name attributes
         return(is.null(names(value)))
      }
  }
)



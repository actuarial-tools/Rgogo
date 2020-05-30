setClass(
   Class = "IObject",
   contains = "VIRTUAL",
   slots = c(
      Id = "character",
      Descrip = "character"
   )
)


setValidity(
   Class = "IObject",
   method = function(object) {
      return(TRUE)
   }
)

setMethod(
   f = "GetId",
   signature = "IObject",
   definition = function(object) {
      return(object@Id)
   }
)


setMethod(
   f = "SetId<-",
   signature = c("IObject", "character"),
   definition = function(object, value) {
      object@Id <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetDescrip",
   signature = "IObject",
   definition = function(object) {
      return(object@Descrip)
   }
)


setMethod(
   f = "SetDescrip<-",
   signature = c("IObject", "character"),
   definition = function(object, value) {
      object@Descrip <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "SaveAsRda",
   signature = "IObject",
   definition = function(object, overwrite = FALSE) {
      stop("Method 'SaveAsRda' must be implemented by class that extends 'IObject'.")
   }
)


# setMethod(
#    f = "Deploy",
#    signature = c("IObject", "logical"),
#    definition = function(object, overwrite = FALSE) {
#       stop("'Deploy' method must be implemented by a class extending 'IObject' virtual class.")
#    }
# )
#
#
# # Helper function for serializing an instance of IObject
# Serialize <- function(object, overwrite, prefix = character(0L)) {
#    stopifnot(HasValue(id <- GetId(object)))
#    if (length(prefix) > 0) {
#       prefix <- ifelse(endsWith(prefix, "."), prefix, paste0(prefix, "."))
#       rdaName <- ifelse(startsWith(id, prefix), id, paste0(prefix, id))
#    } else {
#       rdaName <- id
#    }
#    eval(parse(text = paste(rdaName, "<- object")))
#    eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
# }






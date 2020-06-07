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

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
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 0, maxLen = 1), object@Id)
      if (isValid != TRUE) {
         AddMessage(err) <- "Object identifier must be a character string."
      }
      if (length(object@Id) == 1) {
         isValid <- grepl("^[a-zA-Z][a-zA-Z0-9_.]*$", object@Id)
         if (isValid != TRUE) {
            AddMessage(err) <- "Invalid object identifier.  An identifier must consist of Consist of A-Z, a-z, 0–9, period (“.”) or underscore ('_') only, and must start with A-Z or a-z."
         }
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
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

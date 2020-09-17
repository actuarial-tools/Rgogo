#' @include IObject.R
NULL

setClass(Class = "IPremAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_IPremAssump", members = c("character", "IPremAssump"))

setValidity(
   Class = "IPremAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "PremAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'PremAssump.'"
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
   f = "GetAssumpId",
   signature = "IPremAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("IPremAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "PremAssump.")) {
         value <- paste0("PremAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetAssump",
   signature = "IPremAssump",
   definition = function(object, cov, plan, applyMargin) {
      stop("Method 'GetAssump' must be implemented by a class extending 'IPremAssump'.")
   }
)



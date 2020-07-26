#' @include IObject.R
NULL

setClass(Class = "IExpnsAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_IExpnsAssump", members = c("character", "IExpnsAssump"))

setValidity(
   Class = "IExpnsAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "ExpnsAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'ExpnsAssump.'"
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
   signature = "IExpnsAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("IExpnsAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "ExpnsAssump.")) {
         value <- paste0("ExpnsAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "IExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetExpdAssump' must be implemented by a class extending 'IExpnsAssump'.")
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "IExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetPaddAssump' must be implemented by a class extending 'IExpnsAssump'.")
   }
)

setMethod(
   f = "GetAssump",
   signature = "IExpnsAssump",
   definition = function(object, cov, plan, assumpInfo = list(), projStartDate = NULL) {
      stop("Method 'GetAssump' must be implemented by a class extending 'IExpnsAssump'.")
   }
)


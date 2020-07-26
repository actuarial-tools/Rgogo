setClass(Class = "IIntrAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_IIntrAssump", members = c("character", "IIntrAssump"))

setValidity(
   Class = "IIntrAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "IntrAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'IntrAssump.'"
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
   signature = "IIntrAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("IIntrAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "IntrAssump.")) {
         value <- paste0("IntrAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "IIntrAssump",
   definition = function(object, assumpInfo, ...) {
      stop("Method 'GetExpdAssump' must be implemented by a class inheriting 'IIntrAssump'")
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "IIntrAssump",
   definition = function(object, assumpInfo, ...) {
      stop("Method 'GetPaddAssump' must be implemented by a class inheriting 'IIntrAssump'")
   }
)

setMethod(
   f = "GetAssump",
   signature = "IIntrAssump",
   definition = function(object, assumpInfo = list(), ...) {
      assumpInfo <- GetExpdAssump(object, projLen, assumpInfo, ...)
      assumpInfo <- GetPaddAssump(object, projLen, assumpInfo, ...)
      return(assumpInfo)
   }
)


setClass(Class = "IIntrCredAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_IIntrCredAssump", members = c("character", "IIntrCredAssump"))

setValidity(
   Class = "IIntrCredAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "IntrCredAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'IntrCredAssump.'"
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
   signature = "IIntrCredAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("IIntrCredAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "IntrCredAssump.")) {
         value <- paste0("IntrCredAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetAssump",
   signature = "IIntrCredAssump",
   definition = function(object, cov, plan) {
      stop("Method 'GetAssump' must be implemented by a class extending 'IIntrCredAssump'.")
   }
)


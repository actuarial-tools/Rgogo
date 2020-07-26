#' @include IObject.R
NULL

setClass(Class = "ILapseAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_ILapseAssump", members = c("character", "ILapseAssump"))

setValidity(
   Class = "ILapseAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "LapseAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'LapseAssump.'"
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
   signature = "ILapseAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("ILapseAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "LapseAssump.")) {
         value <- paste0("LapseAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "ILapseAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetExpdAssump' must be implemented by a class extending 'ILapseAssump'.")
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "ILapseAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetPaddAssump' must be implemented by a class extending 'ILapseAssump'.")
   }
)

setMethod(
   f = "GetAssump",
   signature = "ILapseAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetAssump' must be implemented by a class extending 'ILapseAssump'.")
   }
)



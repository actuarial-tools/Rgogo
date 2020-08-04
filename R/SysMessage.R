setClass(Class = "SysMessage", slots = c(Message = "character", Bullet = "character"))

New.SysMessage <- function() {
   msg <- new(Class = "SysMessage", Message = character(0), Bullet = " *** ")
   return(msg)
}

setMethod(
   f = "GetMessage",
   signature = "SysMessage",
   definition = function(object) {
      return(object@Message)
   }
)

setReplaceMethod(
   f = "AddMessage",
   signature = "SysMessage",
   definition = function(object, value) {
      object@Message <- c(object@Message, paste(object@Bullet, value))
      return(object)
   }
)

setMethod(
   f = "NoMessage",
   signature = "SysMessage",
   definition = function(object) {
      return(length(object@Message) == 0)
   }
)



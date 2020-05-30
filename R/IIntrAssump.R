setClass(Class = "IIntrAssump", contains = c("IObject", "VIRTUAL"))


setClassUnion(name = "character_or_IIntrAssump", members = c("character", "IIntrAssump"))


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


setMethod(
   f = "SaveAsRda",
   signature = "IIntrAssump",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(assumpId <- GetId(object)))
      rdaName <- paste0(ifelse(startsWith(assumpId, "IntrAssump."), "", "IntrAssump."), assumpId)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)


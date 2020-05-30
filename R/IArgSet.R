#' @include IObject.R
NULL


setClass(
   Class = "IArgSet",
   contains = c("IObject", "VIRTUAL")
)


setClassUnion(name = "character_or_IArgSet", members = c("character", "IArgSet"))


setMethod(
   f = "GetArgNames",
   signature = "IArgSet",
   definition = function(object) {
      return(slotNames(object))
   }
)


setMethod(
   f = "GetArgValue",
   signature = "IArgSet",
   definition = function(object, argName) {
      return(slot(object, argName))
   }
)


setMethod(
   f = "SetArgValue",
   signature = "IArgSet",
   definition = function(object, ...) {
      valueList <- list(...)
      argNames <- names(valueList)
      for (i in 1:length(valueList)) {
         slot(object, argNames[i]) <- valueList[[i]]
      }
      validObject(object)
      return(object)
   }
)


# setMethod(
#    f = "Deploy",
#    signature = c("IArgSet", "logical"),
#    definition = function(object, overwrite = FALSE) {
#       Serialize(object, overwrite, class(object))
#    }
# )


# setMethod(
#    f = "SaveAsRda",
#    signature = "IArgSet",
#    definition = function(object, overwrite = FALSE) {
#       Deploy(object, overwrite)
#    }
# )



setMethod(
   f = "SaveAsRda",
   signature = "IArgSet",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(id <- GetId(object)))
      rdaName <- paste0(ifelse(startsWith(id, "ArgSet."), "", "ArgSet."), id)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)




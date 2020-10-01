#' @include IObject.R
NULL

setClass(
   Class = "IArgSet",
   contains = c("IObject", "VIRTUAL")
)

setClassUnion(name = "character_or_IArgSet", members = c("character", "IArgSet"))

setValidity(
   Class = "IArgSet",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "ArgSet.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'ArgSet.'"
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
   f = "GetArgSetId",
   signature = "IArgSet",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetArgSetId<-",
   signature = c("IArgSet", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "ArgSet.")) {
         value <- paste0("ArgSet.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

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

setMethod(
   f = "Contains",
   signature = c("IArgSet", "character"),
   definition = function(object, item) {
      return(item %in% slotNames(object))
   }
)

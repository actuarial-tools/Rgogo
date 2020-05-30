#' @include IObject.R
NULL


setClass(Class = "ILapseAssump", contains = c("IObject", "VIRTUAL"))


setClassUnion(name = "character_or_ILapseAssump", members = c("character", "ILapseAssump"))


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



setMethod(
   f = "SaveAsRda",
   signature = "ILapseAssump",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(assumpId <- GetId(object)))
      rdaName <- paste0(ifelse(startsWith(assumpId, "LapseAssump."), "", "LapseAssump."), assumpId)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)


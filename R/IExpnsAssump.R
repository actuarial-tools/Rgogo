#' @include IObject.R
NULL


setClass(Class = "IExpnsAssump", contains = c("IObject", "VIRTUAL"))


setClassUnion(name = "character_or_IExpnsAssump", members = c("character", "IExpnsAssump"))


setGeneric(name = "ProjAcqExpns", def = function(object, ...) {standardGeneric("ProjAcqExpns")})
setGeneric(name = "ProjMntExpns", def = function(object, ...) {standardGeneric("ProjMntExpns")})
setGeneric(name = "ProjMntExpnsPerPol", def = function(object, ...) {standardGeneric("ProjMntExpnsPerPol")})
setGeneric(name = "ProjMntExpnsPerPrem", def = function(object, ...) {standardGeneric("ProjMntExpnsPerPrem")})
setGeneric(name = "ProjMntExpnsPerClaim", def = function(object, ...) {standardGeneric("ProjMntExpnsPerClaim")})


# setMethod(
#    f = "ProjAcqExpns",
#    signature = "IExpnsAssump",
#    definition = function(object, cov, assumpInfo, ...) {
#       stop("Method 'ProjAcqExpns' must be implemented by a class inheriting 'IExpnsAssump'")
#    }
# )
#
#
# setMethod(
#    f = "ProjMntExpns",
#    signature = "IExpnsAssump",
#    definition = function(object, cov, assumpInfo, ...) {
#       stop("Method 'ProjMntExpns' must be implemented by a class inheriting 'IExpnsAssump'")
#    }
# )
#
#
# setMethod(
#    f = "ProjMntExpnsPerPrem",
#    signature = "IExpnsAssump",
#    definition = function(object, cov, assumpInfo, ...) {
#       stop("Method 'ProjMntExpnsPerPrem' must be implemented by a class inheriting 'IExpnsAssump'")
#    }
# )
#
#
# setMethod(
#    f = "ProjMntExpnsPerClaim",
#    signature = "IExpnsAssump",
#    definition = function(object, cov, assumpInfo, ...) {
#       stop("Method 'ProjMntExpnsPerPremAmt' must be implemented by a class inheriting 'IExpnsAssump'")
#    }
# )


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
      # if (is.null(projStartDate)) {
      #    projStartDate <- GetIssDate(cov)
      # }
      # assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo, projStartDate)
      # assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo, projStartDate)
      # return(assumpInfo)
      stop("Method 'GetAssump' must be implemented by a class extending 'IExpnsAssump'.")
   }
)


setMethod(
   f = "SaveAsRda",
   signature = "IExpnsAssump",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(assumpId <- GetId(object)))
      rdaName <- paste0(ifelse(startsWith(assumpId, "ExpnsAssump."), "", "ExpnsAssump."), assumpId)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)


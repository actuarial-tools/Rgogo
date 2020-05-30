#' @include IPlan.AEnd.R
NULL


setClass(
   Class = "IPlan.AEnd.Par",
   contains = "IPlan.AEnd",
   slots = c(PUA = "character")
)


IPlan.AEnd.Par <- function(planId, covYears, premYears = NA, matBenSchd) {
   plan <- new(
      Class = "IPlan.AEnd.Par",
      Id = as.character(planId),
      CovPeriod = c(CovYears = covYears),
      PremPeriod = c(PremYears = ifelse(is.na(premYears), covYears, premYears)),
      MatBenSchd = matBenSchd
   )
   return(plan)
}


setMethod(
   f = "GetPUA",
   signature = "IPlan.AEnd.Par",
   definition = function(object) {
      if (length(object@PUA) > 0) {
         pua <- paste0(ifelse(startsWith(object@PUA, "PUA."), "", "PUA."), object@PUA)
         return(eval(expr = parse(text = pua)))
      } else {
         return(NULL)
      }
   }
)


setMethod(
   f = "SetPUA<-",
   signature = "IPlan.AEnd.Par",
   definition = function(object, value) {
      object@PUA <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "Project",
   signature = "IPlan.AEnd.Par",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      resultContainer <- Project(GetPUA(object), cov, resultContainer)
      return(resultContainer)
   }
)






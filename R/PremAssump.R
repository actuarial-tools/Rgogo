#' @include IPremAssump.R
NULL

setClass(
   Class = "PremAssump",
   contains = "IPremAssump",
   slots = list(
      PremAdj = "numeric",
      Margin = "numeric"
   )
)

PremAssump <- function(premAdj = numeric(0L), margin = 0, id = character(0L), descrip = character(0L)) {
   assump <- new(
      Class = "PremAssump",
      PremAdj = premAdj,
      Margin = margin,
      Descrip = descrip
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}

setMethod(
   f = "GetPremAdj",
   signature = "PremAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@PremAdj)
      }
      if (is.null(plan)) {
         plan <- GetPlan(cov)
      }
      len <- ceiling(GetCovYears(plan, cov))
      if (length(object@PremAdj) == 0) {
         return(rep(1, len))
      } else if (length(object@PremAdj) == 1) {
         return(rep(object@PremAdj, len))
      } else {
         return(FillTail(object@PremAdj, 0, len))
      }
   }
)

setMethod(
   f = "SetPremAdj<-",
   signature = "PremAssump",
   definition = function(object, value) {
      object@PremAdj <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMargin",
   signature = "PremAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@Margin)
      }
      if (is.null(plan)) {
         plan <- GetPlan(cov)
      }
      len <- ceiling(GetCovYears(plan, cov))
      if (length(object@Margin) == 0) {
         return(rep(0, len))
      } else if (length(object@Margin) == 1) {
         return(rep(object@Margin, len))
      } else {
         return(FillTail(object@Margin, 0, len))
      }
   }
)

setMethod(
   f = "SetMargin<-",
   signature = "PremAssump",
   definition = function(object, value) {
      object@Margin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAssump",
   signature = "PremAssump",
   definition = function(object, cov, plan, applyMargin){
      covMonths <- GetCovMonths(plan, cov)
      premAdj <- rep(GetPremAdj(object, cov, plan), each = 12, length.out = covMonths)
      if (applyMargin == TRUE) {
         margin <- rep(GetMargin(object, cov, plan), each = 12, length.out = covMonths)
         return(premAdj * (1 + margin))
      } else {
         return(premAdj)
      }
   }
)




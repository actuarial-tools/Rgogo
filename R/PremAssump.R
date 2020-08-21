#' @include IPremAssump.R
NULL

setClass(
   Class = "PremAssump",
   contains = "IPremAssump",
   slots = list(
      PremAdj = "numeric",
      PremPfad = "numeric"
   )
)

PremAssump <- function(premAdj = numeric(0L), premPfad = 0, id = character(0L), descrip = character(0L)) {
   assump <- new(
      Class = "PremAssump",
      PremAdj = premAdj,
      PremPfad = premPfad,
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
   f = "GetPremPfad",
   signature = "PremAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@PremPfad)
      }
      if (is.null(plan)) {
         plan <- GetPlan(cov)
      }
      len <- ceiling(GetCovYears(plan, cov))
      if (length(object@PremPfad) == 0) {
         return(rep(0, len))
      } else if (length(object@PremPfad) == 1) {
         return(rep(object@PremPfad, len))
      } else {
         return(FillTail(object@PremPfad, 0, len))
      }
   }
)

setMethod(
   f = "SetPremPfad<-",
   signature = "PremAssump",
   definition = function(object, value) {
      object@PremPfad <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "PremAssump",
   definition = function(object, cov, plan, assumpInfo) {
      assumpInfo$PremAdj.Expd <- GetPremAdj(cov, plan)
      return(assumpInfo)
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "PremAssump",
   definition = function(object, cov, plan, assumpInfo) {
      assumpInfo$PremAdj.Padd <- assumpInfo$PremAdj.Expd * (1 + GetPremPfad(object, cov, plan))
      return(assumpInfo)
   }
)


setMethod(
   f = "GetAssump",
   signature = "PremAssump",
   definition = function(object, cov, plan, assumpInfo = list()){
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo)
      return(assumpInfo)
   }
)




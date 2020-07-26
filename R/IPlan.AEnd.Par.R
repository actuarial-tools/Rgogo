#' @include IPlan.AEnd.R
NULL


setClass(
   Class = "IPlan.AEnd.Par",
   contains = "IPlan.AEnd",
   slots = c(PUA = "character")
)


IPlan.AEnd.Par <- function(covYears, premYears = NA,
                           premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                           polFee = numeric(0), premTaxRate = numeric(0L), matBenSchd, cvTable = character(0L), pua = character(0L),
                           commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                           rein = character(0L), planId = character(0L), descrip = character(0L)) {
   plan <- new(
      Class = "IPlan.AEnd.Par",
      CovPeriod = c(CovYears = covYears),
      PremPeriod = c(PremYears = ifelse(is.na(premYears), covYears, premYears)),
      PremTable = premTable,
      ModFactor = modFactor,
      PolFee = polFee,
      MatBenSchd = matBenSchd,
      CVTable = cvTable,
      PUA = pua,
      CommSchd = commSchd,
      OvrdOnPremSchd = ovrdOnPremSchd,
      OvrdOnCommSchd = ovrdOnCommSchd,
      PremTaxRate = premTaxRate,
      Rein = rein,
      Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(planId)
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






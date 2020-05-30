#' @include MortAssump.R
NULL


# MortAssump.A differs from MortAssump in how margin is applied when determining the padded assumption.
# Margin is applied as an multiple extra to the expected assumption; i.e. q.Padd = q.Expd * (1 + pfad).
setClass(Class = "MortAssump.A", contains = c("MortAssump"))


# Consturctor for Mortality Assumption
MortAssump.A <- function(id = character()) {
   assump <- new(
      Class = "MortAssump.A",
      Id = as.character(id),
      MortTableMult = 1,
      ExtraMortTableMult = 1,
      MortImprovRate = 0,
      MortPfad = 0
   )
   return(assump)
}



setMethod(
   f = "GetMargin",
   signature = "MortAssump.A",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      pfad <- GetMortPfad(object, cov, plan)
      assumpInfo$q.Margin <- assumpInfo$q.Expd * pfad
      return(assumpInfo)
   }
)



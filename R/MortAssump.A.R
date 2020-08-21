#' @include MortAssump.R
NULL

# MortAssump.A differs from MortAssump in how margin is applied when determining the padded assumption.
# Margin is applied as an multiple extra to the expected assumption; i.e. q.Padd = q.Expd * (1 + pfad).
setClass(Class = "MortAssump.A", contains = c("MortAssump"))

MortAssump.A <- function(mortTable = character(0L),
                         mortTableMult = 1,
                         extraMortTable = character(0L),
                         extraMortTableMult = 1,
                         mortImprovRate = 0,
                         mortPfad = 0,
                         id = character(0L),
                         descrip = character(0L)) {
   assump <- new(
      Class = "MortAssump.A",
      MortTable = mortTable,
      MortTableMult = mortTableMult,
      ExtraMortTable = extraMortTable,
      ExtraMortTableMult = extraMortTableMult,
      MortImprovRate = mortImprovRate,
      MortPfad = mortPfad,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
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



setClass(
   Class = "ArgSet.PremSolver.IA",
   contains = "ArgSet.PremSolver"
)

setValidity(
   Class = "ArgSet.PremSolver.IA",
   method = function(object) {
      err <- New.SysMessage()
      # Cannot set lapse assumption
      isValid <- length(object@LapseAssump) == 0
      if (isValid != TRUE) {
         AddMessage(err) <- "Lapse assumption cannot be set for immediate annuity."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.PremSolver.IA <- function(projStartDate,
                                 pricIssAge,
                                 pricFaceAmt,
                                 unitFaceAmt,
                                 targProfitMargin,
                                 mortAssump = character(0L),
                                 expnsAssump = character(0L),
                                 intrAssump = character(0L),
                                 applyMortMargin = FALSE,
                                 applyExpnsMargin = FALSE,
                                 applyIntrMargin = FALSE,
                                 digits = 2L,
                                 interval = c(0, 99999),
                                 tolerance = 10^-4,
                                 id = character(0L),
                                 descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.PremSolver.IA",
      ProjStartDate = lubridate::as_date(projStartDate),
      PricIssAge = pricIssAge,
      PricFaceAmt = pricFaceAmt,
      PricPremMode = 1L,
      UnitFaceAmt = unitFaceAmt,
      TargProfitMargin = targProfitMargin,
      Digits = digits,
      MortAssump = mortAssump,
      LapseAssump = character(0L),
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = applyMortMargin,
      ApplyLapseMargin = FALSE,
      ApplyExpnsMargin = applyExpnsMargin,
      ApplyIntrMargin = applyIntrMargin,
      Interval = c(min(interval), max(interval)),
      Tolerance = tolerance,
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}


setClass(
   Class = "ArgSet.PremSolver.IAJS",
   contains = "ArgSet.PremSolver.IA",
   slots = c(
      SurvrAgeDiff = "integer",
      SurvrRiskClass = "character"
   )
)

setValidity(
   Class = "ArgSet.PremSolver.IAJS",
   method = function(object) {
      err <- New.SysMessage()
      # Slot @SurvrAgeDiff must have name attribute.
      # Element names correspond to all possible risk classes of primary insured, and element values are the corresponding age difference of survivor.
      isValid <- Validate(
         Validator.Names(hasNames = TRUE),
         object@SurvrAgeDiff
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @SurvrAgeDiff must have name attribute. Element names correspond to all possible risk classes of primary insured, and element values are the corresponding age difference of survivor."
      }
      # Slot @SurvrRiskClass must have name attribut
      # Element names correspond to all possible risk classes of primary insured, and element values are the corresponding risk classes of survivor.
      isValid <- Validate(
         Validator.Names(hasNames = TRUE),
         object@SurvrRiskClass
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @SurvrRiskClass must have name attribute. Element names correspond to all possible risk classes of primary insured, and element values are the corresponding risk classes of survivor."
      }

   }
)

ArgSet.PremSolver.IAJS <- function(projStartDate,
                                   pricIssAge,
                                   survrAgeDiff = integer(0L),
                                   survrRiskClass = character(0L),
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
      Class = "ArgSet.PremSolver.IAJS",
      ProjStartDate = lubridate::as_date(projStartDate),
      PricIssAge = pricIssAge,
      SurvrAgeDiff = survrAgeDiff,
      SurvrRiskClass = survrRiskClass,
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


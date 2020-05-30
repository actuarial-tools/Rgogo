setClass(
   Class = "ArgSet.PremSolver",
   contains = "ArgSet.DCF",
   slots = c(
      PricIssAge = "integer_or_list",
      PricFaceAmt = "numeric_or_list",
      PricPremMode = "integer",
      UnitFace = "numeric",
      TargProfitMargin = "numeric_or_list",
      Interval = "numeric",
      Tolerance = "numeric",
      Digits = "integer"
   )
)


setValidity(
   Class = "ArgSet.PremSolver",
   method = function(object) {
      err <- New.SysMessage()
      # UnitFace
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = FALSE)
         ),
         object@UnitFace
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@UnitFace' of class 'ArgSet.PremSolver' must be a numeric vector of length 1."
      }
      # TargProfitMargin
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(allowNA = FALSE)
         ),
         object@TargProfitMargin
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@TargProfitMargin' of class 'ArgSet.PremSolver' must be a numeric vector of length 1."
      }
      # Interval: must be a numeric vector of length 2.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 2, maxLen = 2),
            Validator.Range(allowNA = FALSE)
         ),
         object@Interval
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Interval' of class 'ArgSet.PremSolver' must be a numeric vector of length 2."
      }
      # Tolerance: must contain a numeric value.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(allowNA = FALSE)
         ),
         object@Tolerance
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Tolerance' of class 'ArgSet.PremSolver' must contain a numeric scalar."
      }
      # Digits: must contain an integer value.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(allowNA = FALSE)
         ),
         object@Digits
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Digits' of class 'ArgSet.PremSolver' must contain an integer scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


ArgSet.PremSolver <- function(id = character(0L),
                              projStartDate,
                              pricIssAge,
                              pricFaceAmt,
                              pricPremMode,
                              unitFace,
                              targProfitMargin,
                              digits,
                              mortAssump = character(0L),
                              lapseAssump = character(0L),
                              expnsAssump = character(0L),
                              intrAssump = character(0L),
                              applyMortMargin = FALSE,
                              applyLapseMargin = FALSE,
                              applyExpnsMargin = FALSE,
                              applyIntrMargin = FALSE) {
   args <- new(
      Class = "ArgSet.PremSolver",
      Id = id,
      ProjStartDate = as.Date(projStartDate),
      PricIssAge = pricIssAge,
      PricFaceAmt = pricFaceAmt,
      PricPremMode = pricPremMode,
      UnitFace = unitFace,
      TargProfitMargin = targProfitMargin,
      Digits = digits,
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = applyMortMargin,
      ApplyLapseMargin = applyLapseMargin,
      ApplyExpnsMargin = applyExpnsMargin,
      ApplyIntrMargin = applyIntrMargin,
      Interval = c(0, 99999),
      Tolerance = 10^-4
   )
   return(args)
}



setClass(
   Class = "ArgSet.PPM",
   contains = "ArgSet.DCF",
   slots = c(
      ValuDate = "Date",
      ResFloor = "numeric"
   )
)


setValidity(
   Class = "ArgSet.PPM",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@ValuDate)
      if(isValid != TRUE) {
         AddMessage(err) <- "Value of slot '@ValuDate' must be of length 1."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@ResFloor)
      if(isValid != TRUE) {
         AddMessage(err) <- "Value of slot '@ResFloor' must be of length 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


ArgSet.PPM <- function(id = character(0L),
                       valuDate = as.Date("1899-12-31"),
                       mortAssump = character(),
                       lapseAssump = character(),
                       expnsAssump = character(),
                       intrAssump = character(),
                       applyMortMargin = TRUE,
                       applyLapseMargin = TRUE,
                       applyExpnsMargin = TRUE,
                       applyIntrMargin = TRUE,
                       reserveFloor = -Inf){
   args <- new(
      Class = "ArgSet.PPM",
      Id = id,
      ValuDate = lubridate::as_date(valuDate),
      ProjStartDate = lubridate::as_date(valuDate) + 1,
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = applyMortMargin,
      ApplyLapseMargin = applyLapseMargin,
      ApplyExpnsMargin = applyExpnsMargin,
      ApplyIntrMargin = applyIntrMargin,
      ResFloor = reserveFloor
   )
   return(args)
}





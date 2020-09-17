setClass(Class = "ArgSet.PPM.Fund",contains = c("ArgSet.PPM", "ArgSet.DCF.Fund"))

ArgSet.PPM.Fund <- function(valuDate = as.Date("1899-12-31"),
                           mortAssump = character(),
                           lapseAssump = character(),
                           expnsAssump = character(),
                           intrAssump = character(),
                           premAssump = character(),
                           intrCredAssump = character(),
                           applyMortMargin = TRUE,
                           applyLapseMargin = TRUE,
                           applyExpnsMargin = TRUE,
                           applyIntrMargin = TRUE,
                           applyPremMargin = TRUE,
                           reserveFloor = -Inf,
                           id = character(0L),
                           descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.PPM.Fund",
      ValuDate = lubridate::as_date(valuDate),
      ProjStartDate = lubridate::as_date(valuDate) + 1,
      MortAssump = mortAssump,
      LapseAssump = lapseAssump,
      ExpnsAssump = expnsAssump,
      IntrAssump = intrAssump,
      PremAssump = premAssump,
      IntrCredAssump = intrCredAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyLapseMargin = as.logical(applyLapseMargin),
      ApplyExpnsMargin = as.logical(applyExpnsMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      ApplyPremMargin = as.logical(applyPremMargin),
      ResFloor = as.numeric(reserveFloor),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}


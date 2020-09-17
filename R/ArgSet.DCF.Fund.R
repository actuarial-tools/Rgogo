setClass(Class = "ArgSet.DCF.Fund", contains = c("ArgSet.DCF", "ArgSet.CF.Fund"))

ArgSet.DCF.Fund <- function(projStartDate = "1900-01-01",
                            mortAssump = character(0L),
                            lapseAssump = character(0L),
                            expnsAssump = character(0L),
                            intrAssump = character(0L),
                            premAssump = character(0L),
                            intrCredAssump = character(0L),
                            applyMortMargin = FALSE,
                            applyLapseMargin = FALSE,
                            applyExpnsMargin = FALSE,
                            applyIntrMargin = FALSE,
                            applyPremMargin = FALSE,
                            id = character(0L),
                            descrip = character(0L)) {
   object <- new(
      Class = "ArgSet.DCF.Fund",
      ProjStartDate = lubridate::as_date(projStartDate),
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
      Descrip = as.character(descrip)
   )
   SetArgSetId(object) <- as.character(id)
   return(object)
}



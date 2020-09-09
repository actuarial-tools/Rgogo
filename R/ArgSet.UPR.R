setClass(
   Class = "ArgSet.UPR",
   contains = "IArgSet",
   slots = c(
      ValuDate = "Date"
   )
)

ArgSet.UPR <- function(valuDate, id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.UPR",
      ValuDate = lubridate::as_date(valuDate),
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}

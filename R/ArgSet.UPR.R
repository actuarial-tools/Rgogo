setClass(
   Class = "ArgSet.UPR",
   contains = "IArgSet",
   slots = c(
      ValuDate = "Date"
   )
)


ArgSet.UPR <- function(valuDate) {
   args <- new(Class = "ArgSet.UPR", ValuDate = valuDate)
   return(args)
}

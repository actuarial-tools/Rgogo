#' @include ArgSet.NP.R
NULL

setClass(
   Class = "ArgSet.NFB",
   contains = "ArgSet.NP",
   slots = c(
      A = "numeric",
      B = "numeric",
      C = "numeric"
   )
)

setValidity(
   Class = "ArgSet.NFB",
   method = function(object) {
      err <- New.SysMessage()
      # @A: length must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@A)
      if (isValid != TRUE) {
         AddMessage(err) <- "Length of slot value '@A' must be 1."
      }
      # @B: length must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@B)
      if (isValid != TRUE) {
         AddMessage(err) <- "Length of slot value '@B' must be 1."
      }
      # @C: length must be 1.
      isValid = Validate(Validator.Length(minLen = 1, maxLen = 1), object@C)
      if (isValid != TRUE) {
         AddMessage(err) <- "Length of slot value '@C' must be 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ArgSet.NFB <- function(mortAssump = character(0L), intrAssump = character(0L),
                       applyMortMargin = FALSE, applyIntrMargin = FALSE,
                       a = 1.25, b = 0.04, c = 0.01,
                       id = character(0L), descrip = character(0L)) {
   arg <- new(
      Class = "ArgSet.NFB",
      MortAssump = mortAssump,
      IntrAssump = intrAssump,
      ApplyMortMargin = as.logical(applyMortMargin),
      ApplyIntrMargin = as.logical(applyIntrMargin),
      A = a,
      B = b,
      C = c,
      Descrip = as.character(descrip)
   )
   SetArgSetId(arg) <- as.character(id)
   return(arg)
}


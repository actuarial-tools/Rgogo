setClass(
   Class = "CommutFunc",
   contains = "ICommutation",
   slots = c(
      MortTable = "ANY",
      MortTableMult = "numeric",
      IntrRate = "numeric"
   )
)

setValidity(
   Class = "CommutFunc",
   method = function(object) {
      err <- New.SysMessage()
      # @MortTable
      isValid <- is(object@MortTable, "Table.AA") | is(object@MortTable, "Table.SU") | is.character(object@MortTable)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @MortTable must be of one of the following classes: character, Table.AA or Table.SU."
      }
      # @MortTableMult
      isValid <- Validate(
         Validator.Length(minLen = 1, maxLen = 999),
         object@MortTableMult
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @MortTableMult must contain a numeric value of length between 1 and 999."
      }
      # @IntrRate
      isValid <- Validate(
         Validator.Length(minLen = 1, maxLen = 999),
         object@IntrRate
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @IntrRate must contain a numeric value of length between 1 and 999."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

CommutFunc <- function(mortTable, mortTableMult = 1, intrRate = 0) {
   object <- new(
      Class = "CommutFunc",
      MortTable = mortTable,
      MortTableMult = mortTableMult,
      IntrRate = intrRate
   )
   return(object)
}

setMethod(
   f = "Get.q",
   signature = "CommutFunc",
   definition = function(object, entryAge = 0) {
      if (is.character(object@MortTable)) {
         mortTable <- eval(expr = parse(text = object@MortTable))
      } else {
         mortTable <- object@MortTable
      }
      if (is(mortTable, "Table.AA")) {
         stopifnot(entryAge <= GetMaxAge(mortTable))
         lookUpKey <- list(AttAge = entryAge:GetMaxAge(mortTable))
      } else if (is(mortTable, "Table.SU")) {
         stopifnot(entryAge <= GetMaxAttAge(mortTable))
         lookUpKey <- list(IssAge = entryAge)
      } else {
         stop("Invalid mortatlity table class.")
      }
      q <- LookUp(mortTable, lookUpKey)
      if (length(object@MortTableMult) == 1) {
         q <- q * rep(object@MortTableMult, length.out = length(q))
      } else {
         q <- q * FillTail(object@MortTableMult, filler = 1, len = length(q))
      }
      names(q) <- as.character(seq_along(q) + entryAge - 1)
      return(q)
   }
)

setMethod(
   f = "Get.i",
   signature = "CommutFunc",
   definition = function(object, len = NA_integer_) {
      if (is.na(len)) {
         return(object@IntrRate)
      } else {
         if (length(object@IntrRate) == 1) {
            return(rep(object@IntrRate, length.out = len))
         } else {
            return(FillTail(object@IntrRate, filler = 0, len = len))
         }
      }
   }
)


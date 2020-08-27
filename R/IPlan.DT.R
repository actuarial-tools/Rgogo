setClass(
   Class = "IPlan.DT",
   contains = "IPlan.NLT",
   slots = c(
      LoanIntrRate = "numeric",
      LoanIntrRateType = "integer"     #1L: stated rate; 2L: effective annual rate
   )
)

setValidity(
   Class = "IPlan.DT",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @LoanIntrRate
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@LoanIntrRate
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @LoanIntrRate is invalid.  It must be a non-negative scalar."
      }
      # Validate @LoanIntrRateType
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.InList(valuesAllowed = c(1, 2))
         ),
         object@LoanIntrRateType
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @LoanIntrRateType is invalid.  It must be the integer 1L or 2L."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.DT <- function(covYears, premYears = NA,
                     loanIntrRate, loanIntrRateType = 1L,
                     premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                     polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     rein = character(0L), id = character(0L), descrip = character(0L)) {
   covPeriod <- c(CovYears = covYears)
   if (is.na(premYears)) {
      premPeriod <- c(PremYears = covYears)
   } else {
      premPeriod <- c(PremYears = premYears)
   }
   plan <- new(Class = "IPlan.DT",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               LoanIntrRate = loanIntrRate,
               LoanIntrRateType = loanIntrRateType,
               DthBenSchd = numeric(0L),
               DthBenIntraYearMthd = integer(0L),
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               Rein = rein,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetLoanIntrRate",
   signature = "IPlan.DT",
   definition = function(object) {
      return(object@LoanIntrRate)
   }
)

setMethod(
   f = "SetLoanIntrRate<-",
   signature = "IPlan.DT",
   definition = function(object, value) {
      object@LoanIntrRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetLoanIntrRateType",
   signature = "IPlan.DT",
   definition = function(object) {
      return(object@LoanIntrRateType)
   }
)

setMethod(
   f = "SetLoanIntrRateType<-",
   signature = "IPlan.DT",
   definition = function(object, value) {
      object@LoanIntrRateType = value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetDthBenSchd",
   signature = "IPlan.DT",
   definition = function(object, cov) {
      covMonths <- GetCovMonths(object, cov)
      loanAmortInfo <- LoanAmort(
         loanAmt = 1,
         amortMonths = GetCovMonths(object, cov),
         intrRate = GetLoanIntrRate(object),
         intrRateType = GetLoanIntrRateType(object)
      )
      return(loanAmortInfo$LoanBalance)
   }
)

setMethod(
   f = "SetDthBenSchd<-",
   signature = "IPlan.DT",
   definition = function(object, value) {
      stop("Method 'SetDthBenSchd<-' cannot be invoked by an object of class 'IPlan.DT'.")
   }
)

setMethod(
   f = "GetDthBenIntraYearMthd",
   signature = "IPlan.DT",
   definition = function(object) {
      stop("Method 'GetDthBenIntraYearMthd' cannot be invoked by an object of class 'IPlan.DT'.")
   }
)

setMethod(
   f = "SetDthBenIntraYearMthd<-",
   signature = "IPlan.DT",
   definition = function(object, value) {
      stop("Method 'SetDthBenIntraYearMthd<-' cannot be invoked by an object of class 'IPlan.DT'.")
   }
)



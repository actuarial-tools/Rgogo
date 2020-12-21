#' @include IPlan.End.R
NULL

setClass(
   Class = "IPlan.End.Par",
   contains = "IPlan.End",
   slots = c(PUA = "character")
)

setValidity(
   Class = "IPlan.End.Par",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Length(minLen = 0L, maxLen = 1L), object@PUA)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@PUA' must contain a character vector of length not greater than 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.End.Par <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                          premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                          polFee = numeric(0), premTaxRate = numeric(0L), cvTable = character(0L), pua = character(0L),
                          commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                          rein = character(0L), id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.End.Par",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CVTable = cvTable,
               PUA = pua,
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
   f = "GetPUA",
   signature = "IPlan.End.Par",
   definition = function(object) {
      if (length(object@PUA) > 0) {
         pua <- paste0(ifelse(startsWith(object@PUA, "PUA."), "", "PUA."), object@PUA)
         return(eval(expr = parse(text = pua)))
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetPUA<-",
   signature = "IPlan.End.Par",
   definition = function(object, value) {
      object@PUA <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.End.Par",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      resultContainer <- Project(GetPUA(object), cov, resultContainer)
      return(resultContainer)
   }
)






#' @include IPlan.LT
NULL

setClass(
   Class = "IPlan.LT.ROPM",
   contains = "IPlan.LT",
   slots = c(
      ROPMat = "numeric",
      ROPBasis = "integer"
      # 1L: based on annualized premium;
      # 2L: based on annual premium;
      # 3L: based on annualized premium excluding policy fee;
      # 4L: based on annual premium excluding policy fee
   )
)

setValidity(
   Class = "IPlan.LT.ROPM",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @ROPMat
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@ROPMat
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot 'ROPMat' in 'IPlan.LT.ROPM' class must be a non-negative numeric scalar."
      }
      # Validate ROPBasis
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.InList(valuesAllowed = c(1, 2, 3, 4))
         ),
         object@ROPBasis
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot 'ROPBasis' in 'IPlan.LT.ROPM' class must be an integer of value 1L, 2L, 3L or 4L."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.LT.ROPM <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                          ropMat = 0, ropBasis = 1L,
                          premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                          polFee = numeric(0), premTaxRate = numeric(0L),
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
   plan <- new(Class = "IPlan.LT.ROPM",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               ROPMat = as.numeric(ropMat),
               ROPBasis = ropBasis,
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
   f = "GetROPMat",
   signature = "IPlan.LT.ROPM",
   definition = function(object) {
      return(object@ROPMat)
   }
)

setMethod(
   f = "SetROPMat<-",
   signature = "IPlan.LT.ROPM",
   definition = function(object, value) {
      object@ROPMat <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetROPBasis",
   signature = "IPlan.LT.ROPM",
   definition = function(object) {
      return(object@ROPBasis)
   }
)

setMethod(
   f = "SetROPBasis<-",
   signature = "IPlan.LT.ROPM",
   definition = function(object, value) {
      object@ROPBasis <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.LT.ROPM",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      premMode <- GetPremMode(cov)
      ropBasis <- GetROPBasis(object)
      totalPrem <- sum(resultContainer$Proj$Prem)
      totalPolFee <- sum((resultContainer$Proj$Prem != 0) * GetPolFee(object, premMode))
      if (ropBasis == 1L || ropBasis == 3L) {
         # 1L: ROP is calculated based on total annualized premium (i.e. total premium payable).
         # 3L: ROP is calculated based on total annualized premium excluding policy fee
         ropAmt <- (totalPrem - ifelse(ropBasis == 3L, totalPolFee, 0)) * GetROPMat(object)
      } else if (ropBasis == 2L || ropBasis == 4L) {
         # 2L: ROP is calculated based on total annual premium
         # 4L: ROP is calculated based on total annual premium excluding policy fee
         totalAnnPremExclPolFee <- (totalPrem - totalPolFee) / (premMode * GetModFactor(object, premMode)) * GetModFactor(object, 1)
         totalAnnPolFee <- totalPolFee / (premMode * GetPolFee(object, premMode)) * GetPolFee(object, 1)
         ropAmt <- (totalAnnPremExclPolFee + totalAnnPolFee *(ropBasis == 2L)) * GetROPMat(object)
      } else {
         stop("Invalid ROPBasis")   # This case should not happen.
      }
      # Refund occurs at the end of coverage period.
      if (ropAmt != 0) {
         matBen <- c(rep(0, covMonths - 1), ropAmt)
         resultContainer$Proj$Ben.Mat <- matBen
      }
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.LT.ROPM",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      return(resultContainer)
   }
)




setClass(
   Class = "IPlan.NLT",
   contains = "IPlan.LT",
   slots = c(
      DthBenSchd = "numeric",
      DthBenIntraYearMthd = "integer"    # 0L - death benefit stays constant during a policy year; 1L - mid-year death benefit is interpolated by policy month.
   )
)

setValidity(
   Class = "IPlan.NLT",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.InList(c(0, 1)), object@DthBenIntraYearMthd)
      if (isValid != TRUE) {
         AddMessage(err) <- "@DthBenIntraYearMthd slot value is invalid.  It must be 0L (death benefit stays constant during a policy year) or 1L (mid-year death benefit is interpolated by policy month)."
      }
      if (NoMessage(err)) return(TRUE) else return(GetMessage(err))
   }
)

IPlan.NLT <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                      premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                      polFee = numeric(0), commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                      premTaxRate = numeric(0L), rein = character(0L), planId = character(0L), descrip = character(0L),
                      dthBenSchd, dthBenIntraYearMthd = 0L) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.NLT",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               PremTaxRate = premTaxRate,
               Rein = rein,
               Id = as.character(planId),
               Descrip = as.character(descrip),
               DthBenSchd = dthBenSchd,
               DthBenIntraYearMthd = dthBenIntraYearMthd
   )
   return(plan)
}


setMethod(
   f = "GetDthBenSchd",
   signature = "IPlan.NLT",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {return(object@DthBenSchd)}
      covMonths <- GetCovMonths(object, cov)
      dthBenSchd <- GetDthBenSchd(object)
      intraYearMthd <- GetDthBenIntraYearMthd(object)
      if (intraYearMthd == 0L) {
         # Death benefit remains constant during the middle of a year.
         d <- FillZeroIfNA(rep(dthBenSchd, each = 12), covMonths )
      } else if (intraYearMthd == 1L) {
         # Intra-year death benefit is interpolated by policy months.
         schd <- c(dthBenSchd, 0)
         d <- numeric()
         for(i in (1:length(dthBenSchd))) {
            d <- c(d, seq(from = schd[i], to = schd[i+1], length = 13)[1:12])
         }
         d <- FillZeroIfNA(d, covMonths)
      } else {
         stop("DthBenIntraYearMthd has an invalid value.")
      }
      return(c(d, 0))
   }
)


setMethod(
   f = "SetDthBenSchd<-",
   signature = "IPlan.NLT",
   definition = function(object, value) {
      object@DthBenSchd <- as.numeric(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetDthBenIntraYearMthd",
   signature = "IPlan.NLT",
   definition = function(object) {
      return(object@DthBenIntraYearMthd)
   }
)


setMethod(
   f = "SetDthBenIntraYearMthd<-",
   signature = "IPlan.NLT",
   definition = function(object, value) {
      object@DthBenIntraYearMthd <- as.integer(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "ProjDthBen",
   signature = "IPlan.NLT",
   definition = function(object, cov, resultContainer) {
      dthBen <- GetFaceAmt(cov) * GetDthBenSchd(object, cov)
      resultContainer %<>% AddProjection(projItem = "Ben.Dth", projValue = dthBen)
      return(resultContainer)
   }
)








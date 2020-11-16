# Immediate Annuity
setClass(
   Class = "IPlan.IA",
   contains = "IPlan",
   slots = c(
      AnuPeriod = "numeric",
      PremTable = "character",
      PolFee = "numeric",
      CommSchd = "numeric",
      OvrdOnPremSchd = "numeric",
      OvrdOnCommSchd = "numeric",
      PremTaxRate = "numeric",
      AnuMode = "integer",
      AnuTiming = "integer"
   )
)

setValidity(
   Class = "IPlan.IA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @AnuPeriod
      vg <- ValidatorGroup(
         Validator.Length(minLen = 1, maxLen = 2),
         Validator.Range(minValue = 0, maxValue = 120, allowNA = FALSE),
         Validator.Names(hasNames = TRUE, namesAllowed = c("AnuYears", "AnuToAge"))
      )
      if (Validate(vg, object@CovPeriod) != TRUE) {
         AddMessage(err) <- "Invalid annuity period setting."
      }
      # Validate premium table
      if (length(object@PremTable) > 1){
         vg <- ValidatorGroup(
            Validator.Names(hasNames = TRUE)
         )
      } else {
         vg <- ValidatorGroup(
            # Validator.OfClass(c("character", "ITable")),
            Validator.Names(hasNames = FALSE)
         )
      }
      if (Validate(vg, object@PremTable) != TRUE) {
         AddMessage(err) <- "Invalid premium table setting."
      }
      #Validate @PolFee
      isValid <- Validate(
         Validator.Range(minValue = 0),
         object@PolFee
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Policy fee cannot be less than zero."
      }
      # Validate @CommSchd, @OvrdOnPremSchd and @OvrdOnCommSchd
      vg <- ValidatorGroup(
         Validator.Range(minValue = 0, maxValue = 1, allowNA = FALSE)
      )
      if (Validate(vg, object@CommSchd) != TRUE) {
         AddMessage(err) <- "Invaid commission schedule. Rates must be between 0 and 1."
      }
      if (Validate(vg, object@OvrdOnPremSchd) != TRUE) {
         AddMessage(err) <- "Invaid override on premium schedule. Rates must be between 0 and 1."
      }
      if (Validate(vg, object@OvrdOnCommSchd) != TRUE) {
         AddMessage(err) <- "Invaid override on commission schedule. Rates must be between 0 and 1."
      }
      # Validate @PremTaxRate
      vg <- ValidatorGroup(
         Validator.Length(minLen = 0, maxLen = 1),
         Validator.Range(minValue = 0, maxValue = 1, allowNA = FALSE)
      )
      if (Validate(vg, object@PremTaxRate) != TRUE) {
         AddMessage(err) <- "Invaid premium tax rate.  Tax rate must be a numeric scalar between 0 and 1."
      }
      # Validate annuity mode @AnuMode
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.InList(valuesAllowed = c(1L, 2L, 4L, 12L))
         ),
         object@AnuMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of annuity mode is invalid.  It must be 1L (annual), 2L (semiannual), 4L (quarterly) or 12L (monthly)."
      }
      # Validate annuity timing @AnuTiming
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1L, maxLen = 1L),
            Validator.InList(valuesAllowed = c(0L, 1L))
         ),
         object@AnuTiming
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of annuity timing is invalid.  It must be 0L (beginning of policy month) or 1L (end of policy month)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.IA <- function(anuYears = NA, anuToAge = NA,
                     anuMode = 12L, anuTiming = 0L,
                     premTable = character(0L), polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(anuYears, anuToAge))))
   stopifnot(length(polFee) <= 1)
   anuPeriod <- c(AnuYears = anuYears, AnuToAge = as.integer(anuToAge))
   anuPeriod <- anuPeriod[!is.na(anuPeriod)]
   plan <- new(Class = "IPlan.IA",
               AnuPeriod = anuPeriod,
               AnuMode = anuMode,
               AnuTiming = anuTiming,
               PremTable = premTable,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetRiskClass",
   signature = "IPlan.IA",
   definition = function (object, cov) {
      return(GetRiskClass(cov))
   }
)

setMethod(
   f = "GetCovYears",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@AnuPeriod["AnuYears"]), Inf, object@AnuPeriod["AnuYears"])
      years2 <- ifelse(is.na(object@AnuPeriod["AnuToAge"]), Inf, object@AnuPeriod["AnuToAge"] - GetIssAge(cov))
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetPremYears",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      return(1/12)
   }
)

setMethod(
   f = "GetPremTable",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      if (length(object@PremTable) == 0) {
         return(NULL)
      }
      if (length(object@PremTable) == 1) {
         tblId <- object@PremTable
      } else {
         riskClass <- GetRiskClass(object, cov)
         tblId <- object@PremTable[riskClass]
      }
      tblId <- ifelse(startsWith(tblId, "Prem."), tblId, paste0("Prem.", tblId))
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "SetPremTable<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@PremTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremRate",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      premTable <- GetPremTable(object, cov)
      if (is.null(premTable)) {
         return(NA_real_)
      } else {
         return(LookUp(premTable, cov))
      }
   }
)

setMethod(
   f = "GetPolFee",
   signature = "IPlan.IA",
   definition = function(object) {
      if (length(object@PolFee0 == 0)) {
         return(0)
      } else {
         return(object@PolFee)
      }
   }
)

setMethod(
   f = "SetPolFee<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      stopifnot(length(value) <= 1)
      object@PolFee <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      if (HasValue(object@CommSchd)) {
         comm <- FillZeroIfNA(rep(object@CommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         comm <- rep(0, len = GetCovMonths(object, cov))
      }
      return(comm)
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@CommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      if (HasValue(object@OvrdOnCommSchd)) {
         ovrd <- FillZeroIfNA(rep(object@OvrdOnCommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@OvrdOnCommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      if (HasValue(object@OvrdOnPremSchd)) {
         ovrd <- FillZeroIfNA(rep(object@OvrdOnPremSchd, each = 12), GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@OvrdOnPremSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremTaxRate",
   signature = "IPlan.IA",
   definition = function(object, cov) {
      if (HasValue(object@PremTaxRate)) {
         return(object@PremTaxRate)
      } else {
         return(0)
      }
   }
)

setMethod(
   f = "SetPremTaxRate<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@PremTaxRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAnuMode",
   signature = "IPlan.IA",
   definition = function(object) {
      # Annuity due (payable at the beginning of period): object@AnuMode == 0L
      # Annuity immediate (payable at the end of period): object@AnuMode == 1L
      return(object@AnuMode)
   }
)

setMethod(
   f = "SetAnuMode<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@AnuMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAnuTiming",
   signature = "IPlan.IA",
   definition = function(object) {
      return(object@AnuTiming)
   }
)

setMethod(
   f = "SetAnuTiming<-",
   signature = "IPlan.IA",
   definition = function(object, value) {
      object@AnuTiming <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      # If cov contains modal premium information (i.e. cov@ModPrem contains a value), use that information to preject premium;
      # otherwise, look up premium table to calculate the modal premium.
      # cov@PremMode is ignored.
      singlePrem <- GetModPrem(cov)
      if (!HasValue(singlePrem)) {
         premRate <- GetPremRate(object, cov)
         stopifnot(!is.na(premRate))
         singlePrem <- premRate[1] * GetFaceAmt(cov) + GetPolFee(object)
      }
      prem <- c(singlePrem, rep(0, length.out = GetCovMonths(object, cov) - 1))
      premTax <- prem * GetPremTaxRate(object, cov)
      if (!all(prem == 0)) {
         resultContainer$Proj$Prem = prem
      }
      if (!all(premTax == 0)) {
         resultContainer$Proj$Prem.Tax <- premTax
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      commSchd <- GetCommSchd(object, cov)
      ovrdOnPremSchd <- GetOvrdOnPremSchd(object, cov)
      ovrdOnCommSchd <- GetOvrdOnCommSchd(object, cov)
      if (!is.null(resultContainer$Proj$Prem)) {
         comm <- resultContainer$Proj$Prem * GetCommSchd(object, cov)
         ovrd <- resultContainer$Proj$Prem * GetOvrdOnPremSchd(object, cov) + comm * GetOvrdOnCommSchd(object, cov)
         if (!all(comm == 0)) {
            resultContainer$Proj$Comm <- comm
         }
         if (!all(ovrd == 0)) {
            resultContainer$Proj$Comm.Ovrd <- ovrd
         }
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      anuMode <- GetAnuMode(object)
      a <- rep(GetFaceAmt(cov) / anuMode, length.out = covMonths)   # Face amount of coverage contrains annualized annuity benefit information.
      if (GetAnuTiming(object) == 0) {
         m <- (seq(from = 1, to = length(a)) - 1) %% (12 / anuMode) == 0
      } else {
         m <- (seq(from = 1, to = length(a))) %% (12 / anuMode) == 0
      }
      resultContainer$Proj$Ben.Anu <-  a * m
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.IA",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      return(resultContainer)
   }
)




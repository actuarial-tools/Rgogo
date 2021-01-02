#' @include IPlan.R
NULL

setClass(
   Class = "IPlan.LT",
   contains = c("IPlan"),
   slots = c(
      CovPeriod = "numeric",
      PremPeriod = "numeric",
      PremTable = "character",
      ModFactor = "numeric",
      PolFee = "numeric",
      CommSchd = "numeric",
      OvrdOnPremSchd = "numeric",
      OvrdOnCommSchd = "numeric",
      PremTaxRate = "numeric",
      Rein = "character"
   )
)

setValidity(
   Class = "IPlan.LT",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @CovPeriod
      vg <- ValidatorGroup(
         Validator.Length(minLen = 1, maxLen = 2),
         Validator.Range(minValue = 0, maxValue = 120, allowNA = FALSE),
         Validator.Names(hasNames = TRUE, namesAllowed = c("CovYears", "CovToAge"))
      )
      if (Validate(vg, object@CovPeriod) != TRUE) {
         AddMessage(err) <- "Invalid coverage period setting."
      }
      # Validate @PremPeriod
      vg <- ValidatorGroup(
         Validator.Length(minLen = 1, maxLen = 2),
         Validator.Range(minValue = 0, maxValue = 120, allowNA = FALSE),
         Validator.Names(hasNames = TRUE, namesAllowed = c("PremYears", "PremToAge"))
      )
      if (Validate(vg, object@PremPeriod) != TRUE) {
         AddMessage(err) <- "Invalid premium period setting."
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
      # Validate @ModFactor
      vg <- ValidatorGroup(
         Validator.Names(hasNames = TRUE, namesAllowed = c("1", "2", "4", "12")),
         Validator.Range(minValue = 0)
      )
      if (Validate(vg, object@ModFactor) != TRUE) {
         AddMessage(err) <- "Invalid modal factor setting."
      }
      #Validate @PolFee
      if (length(object@PolFee) > 1) {
         vg <- ValidatorGroup(
            Validator.Range(minValue = 0),
            Validator.Names(hasNames = TRUE, namesAllowed = c("1", "2", "4", "12"))
         )
      } else {
         vg <- ValidatorGroup(
            Validator.Range(minValue = 0),
            Validator.Names(hasNames = FALSE)
         )
      }
      if (Validate(vg, object@PolFee) != TRUE) {
         AddMessage(err) <- "Invalid policy fee setting."
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
      #Validate @Rein
      vg <- Validator.Names(hasNames = (length(object@Rein) >= 1))
      if (Validate(vg, object@Rein) != TRUE) {
         AddMessage(err) <- "Invalid reinsurance setting.  The character vector must have name attribute to indicate the effective date of the treaty."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.LT <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
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
   plan <- new(Class = "IPlan.LT",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
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
   f = "GetRiskClass",
   signature = "IPlan.LT",
   definition = function (object, cov) {
      return(GetRiskClass(cov))
   }
)

setMethod(
   f = "GetCovYears",
   signature = "IPlan.LT",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@CovPeriod["CovYears"]), Inf, object@CovPeriod["CovYears"])
      years2 <- ifelse(is.na(object@CovPeriod["CovToAge"]), Inf, object@CovPeriod["CovToAge"] - GetIssAge(cov))
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetPremYears",
   signature = "IPlan.LT",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@PremPeriod["PremYears"]), Inf, object@PremPeriod["PremYears"])
      years2 <- ifelse(is.na(object@PremPeriod["PremToAge"]), Inf, object@PremPeriod["PremToAge"] - GetIssAge(cov))
      premYears <- min(years1, years2)
      return(premYears)
   }
)

setMethod(
   f = "GetPremTable",
   signature = "IPlan.LT",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@PremTable)
      }
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
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@PremTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremRate",
   signature = "IPlan.LT",
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
   f = "GetModFactor",
   signature = "IPlan.LT",
   definition = function(object, premMode) {
      if (HasValue(object@ModFactor)) {
         return(as.numeric(object@ModFactor[as.character(premMode)]))
      } else {
         return(1 / premMode)
      }
   }
)

setMethod(
   f = "SetModFactor<-",
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@ModFactor <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPolFee",
   signature = "IPlan.LT",
   definition = function(object, premMode) {
      if (HasValue(object@PolFee)) {
         if (length(object@PolFee) == 1) {
            return(object@PolFee * GetModFactor(object, premMode))
         } else {
            return(as.numeric(object@PolFee[as.character(premMode)]))
         }
      } else {
         return(0)
      }
   }
)

setMethod(
   f = "SetPolFee<-",
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@PolFee <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "IPlan.LT",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CommSchd)
      }
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
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@CommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPlan.LT",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnCommSchd)
      }
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
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@OvrdOnCommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPlan.LT",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnPremSchd)
      }
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
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@OvrdOnPremSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremTaxRate",
   signature = "IPlan.LT",
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
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@PremTaxRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRein",
   signature = "IPlan.LT",
   definition = function(object, cov) {
      if (HasValue(object@Rein)) {
         rein <- object@Rein[sort(names(object@Rein), decreasing = TRUE)]
         treatyID <- rein[GetIssDate(cov) >= as.Date(names(rein))][1]
         if (!is.na(treatyID)) {
            treaty <- eval(parse(text = paste0(ifelse(startsWith(treatyID, "Rein."),"","Rein."), treatyID)))
            return(treaty)
         } else {
            return(NULL)
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetRein<-",
   signature = "IPlan.LT",
   definition = function(object, value) {
      object@Rein <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetModPrem",
   signature = "IPlan.LT",
   definition = function(object, cov) {
      premMode <- GetPremMode(cov)
      premRate <- GetPremRate(object, cov)
      stopifnot(!is.na(premRate))
      modPrem <- premRate[1] * GetFaceAmt(cov) * GetModFactor(object, premMode) + GetPolFee(object, premMode)
      return(modPrem)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.LT",
   definition = function(object, cov, resultContainer) {
      # If cov contains modal premium information (i.e. cov@ModPrem contains a value), use that information to preject premium;
      # otherwise, look up premium table to calculate the modal premium.
      modPrem <- GetModPrem(cov)
      premMode <- GetPremMode(cov)
      if (!HasValue(modPrem)) {
         modPrem <- GetModPrem(object, cov)
      }
      prem <- FillZeroIfNA(rep(c(modPrem, rep(0, times = (12 / premMode - 1))), length.out = GetPremMonths(object, cov)), len = GetCovMonths(object, cov))
      premTax <- prem * GetPremTaxRate(object, cov)
      if (!all(prem == 0)) {
         resultContainer$Proj$Prem <- prem
      }
      if (!all(premTax == 0)) {
         resultContainer$Proj$Prem.Tax <- premTax
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPlan.LT",
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
   f = "ProjDthBen",
   signature = "IPlan.LT",
   definition = function(object, cov, resultContainer) {
      faceAmt <- GetFaceAmt(cov)
      if (faceAmt != 0) {
         covMonths <- GetCovMonths(object, cov)
         benDth <- rep(faceAmt, covMonths)
         resultContainer$Proj$Ben.Dth <- benDth
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjRein",
   signature = "IPlan.LT",
   definition = function(object, cov, resultContainer) {
      rein <- GetRein(object, cov)
      if (!is.null(rein)) resultContainer <- Project(rein, cov, resultContainer)
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.LT",
   definition = function(object, cov, resultContainer = list()) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      return(resultContainer)
   }
)

# A function to create an instance of data.frame to store projection information in result container.
NewProjection <- function(resultContainer, cov, plan) {
   stopifnot(is.list(resultContainer), is.null(resultContainer$Proj))
   timeline <- GetIssDate(cov) %m+% months(0:(GetCovMonths(plan, cov) - 1))
   resultContainer$Proj <- list(Timeline = paste0(year(timeline), "-", sprintf("%02d",month(timeline))))
   return(resultContainer)
}




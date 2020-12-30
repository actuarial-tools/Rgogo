# This may be replaced by IPlan.Fund.R

setClass(
   Class = "IPlan.Fund.A",
   contains = "IPlan.Fund",
   slots = c(
      PremLoadSchd = "numeric",
      ExpnsChrgSchd = "numeric",
      ExpnsChrgMode = "integer",
      ExpnsChrgTiming = "integer",
      MinIntrCredRate = "numeric",
      SurChrgSchd = "numeric",
      CommSchd = "numeric",
      OvrdOnPremSchd = "numeric",
      OvrdOnCommSchd = "numeric",
      PremTaxRate = "numeric"
   )
)

setValidity(
   Class = "IPlan.Fund.A",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @PremLoadSchd
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@PremLoadSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid premium load.  Rates must be between 0 and 1."
      }
      # Validate @CommSchd, @OvrdOnPremSchd and @OvrdOnCommSchd
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@CommSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid commission schedule. Rates must be between 0 and 1."
      }
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@OvrdOnPremSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid override on premium schedule. Rates must be between 0 and 1."
      }
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@OvrdOnCommSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid override on commission schedule. Rates must be between 0 and 1."
      }
      # Validate @ExpnsChrgSchd
      isValid <- Validate(Validator.Range(minValue = 0), object@ExpnsChrgSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge schedule. Rates must be between 0 and 1."
      }
      # Validate @ExpnsChrgMode
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.InList(valuesAllowed = c(1, 2, 4, 12))
         ), object@ExpnsChrgMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge mode.  It must be 1, 2, 4 or 12."
      }
      # Validate @ExpnsChrgTiming
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.InList(valuesAllowed = c(0, 1))
         ), object@ExpnsChrgTiming
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge mode.  It must be 0 or 1."
      }
      # Validate @SurChrgSchd
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@SurChrgSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid surrender charge schedule.  The rates must be between 0 and 1."
      }
      # Validate @PremTaxRate
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = 1, allowNA = FALSE)
         ), object@PremTaxRate
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid premium tax rate.  Tax rate must be a numeric scalar between 0 and 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage)
      }
   }
)

IPlan.Fund.A <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA, premLoadSchd = numeric(0L),
                         expnsChrgSchd = numeric(0L), expnsChrgMode = integer(0L), expnsChrgTiming = integer(0L),
                         surChrgSchd = numeric(0L), minIntrCredRate = numeric(0L),
                         commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                         premTaxRate, id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.Fund.A",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremLoadSchd = premLoadSchd,
               ExpnsChrgSchd = expnsChrgSchd,
               ExpnsChrgMode = as.integer(expnsChrgMode),
               ExpnsChrgTiming = as.integer(expnsChrgTiming),
               SurChrgSchd = surChrgSchd,
               MinIntrCredRate = minIntrCredRate,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               PremTaxRate = premTaxRate,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetPremLoadSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@PremLoadSchd)
      } else if (length(object@PremLoadSchd) > 0) {
         premLoad <- FillZeroIfNA(rep(object@PremLoadSchd, each = 12), GetCovMonths(object, cov))
      } else {
         premLoad <- rep(0, len = GetCovMonths(object, cov))
      }
      return(premLoad)
   }
)

setMethod(
   f = "SetPremLoadSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@PremLoadSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@ExpnsChrgSchd)
      } else if (length(object@ExpnsChrgSchd) > 0) {
         expnsChrg <- FillZeroIfNA(rep(object@ExpnsChrgSchd, each = 12), GetCovMonths(object, cov))
      } else {
         expnsChrg <- rep(0, len = GetCovMonths(object, cov))
      }
      return(expnsChrg)
   }
)

setMethod(
   f = "SetExpnsChrgSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@ExpnsChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgMode",
   signature = "IPlan.Fund.A",
   definition = function(object) {
      return(ifelse(length(object@ExpnsChrgMode) > 0, object@ExpnsChrgMode, 1L))
   }
)

setMethod(
   f = "SetExpnsChrgMode<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@ExpnsChrgMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgTiming",
   signature = "IPlan.Fund.A",
   definition = function(object) {
      return(ifelse(length(object@ExpnsChrgTiming) > 0, object@ExpnsChrgTiming, 0L))
   }
)

setMethod(
   f = "SetExpnsChrgTiming<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@ExpnsChrgTiming <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetSurChrgSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@SurChrgSchd)
      } else if (length(object@SurChrgSchd) > 0) {
         surChrg <- FillZeroIfNA(rep(object@SurChrgSchd, each = 12), GetCovMonths(object, cov))
      } else {
         surChrg <- rep(0, len = GetCovMonths(object, cov))
      }
      return(surChrg)
   }
)

setMethod(
   f = "SetSurChrgSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@SurChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMinIntrCredRate",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@MinIntrCredRate)
      } else {
         covMonths <- GetCovMonths(object, cov)
         if (length(object@MinIntrCredRate) == 0) {
            return(rep(-Inf, legnth.out = covMonths))
         } else if (length(object@MinIntrCredRate) == 1) {
            return(rep(object@MinIntrCredRate, length.out = covMonths))
         } else {
            return(FillTail(rep(object@MinIntrCredRate, each = 12), filler = -Inf, len = covMonths))
         }
      }
   }
)

setMethod(
   f = "SetMinIntrCredRate<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@MinIntrCredRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIntrCredDate",
   signature = "IPlan.Fund.A",
   definition = function(object, cov) {
      return(GetIssDate(cov) %m+% months(1:GetCovMonths(object, cov)))
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CommSchd)
      } else if (length(object@CommSchd) > 0) {
         comm <- FillZeroIfNA(rep(object@CommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         comm <- rep(0, len = GetCovMonths(object, cov))
      }
      return(comm)
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@CommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnCommSchd)
      } else if (length(object@OvrdOnCommSchd) > 0) {
         ovrd <- FillZeroIfNA(rep(object@OvrdOnCommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@OvrdOnCommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPlan.Fund.A",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnPremSchd)
      } else if (HasValue(object@OvrdOnPremSchd)) {
         ovrd <- FillZeroIfNA(rep(object@OvrdOnPremSchd, each = 12), GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@OvrdOnPremSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremTaxRate",
   signature = "IPlan.Fund.A",
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
   signature = "IPlan.Fund.A",
   definition = function(object, value) {
      object@PremTaxRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.Fund.A",
   definition = function(object, cov, resultContainer) {
      resultContainer <- callNextMethod()
      if (GetPremTaxRate(object, cov) != 0) {
         resultContainer$Proj$Prem.Tax <- resultContainer$Proj$Prem * GetPremTaxRate(object, cov)
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjFund",
   signature = "IPlan.Fund.A",
   definition = function(object, cov, resultContainer) {
      stopifnot(!is.null(resultContainer$Proj$Prem))
      covMonths <- GetCovMonths(object, cov)
      prem <- resultContainer$Proj$Prem
      premLoad <- -prem * GetPremLoadSchd(object, cov)
      expnsChrg <- -GetExpnsChrgSchd(object, cov) * (((1:covMonths) - 1) %% (12 / GetExpnsChrgMode(object)) == 0)
      iMin <- GetMinIntrCredRate(object, cov)
      if (is.null(resultContainer$.ArgSet)) {
         i <- iMin
         projStartPolMonth <- 1
      } else {
         i <- GetAssump(GetIntrCredAssump(resultContainer$.ArgSet), cov, object)
         i <- ifelse(i < iMin, iMin, i)
         projStartPolMonth <- floor(GetPolMonth(GetIssDate(cov), GetProjStartDate(resultContainer$.ArgSet), exact = TRUE))
      }
      j <- (1 + i) ^ (1 / 12) - 1
      accBal <- GetAccBal(cov)
      fundAdj <- iCred <- fundBeg <- fundEnd <- rep(0, covMonths)
      expnsChrgTiming <- GetExpnsChrgTiming(object)
      for (t in 1:covMonths) {
         fundBeg[t] <- ifelse(t == 1, 0, fundEnd[t - 1])
         openBal <- fundBeg[t] + prem[t] + premLoad[t] + expnsChrg[t] * (expnsChrgTiming == 0)
         iCred[t] <- openBal * j[t]
         fundEnd[t] <- openBal + iCred[t] + expnsChrg[t] * (expnsChrgTiming == 1)
         fundAdj[t] <- (accBal - fundEnd[t]) * (t == (projStartPolMonth - 1))
         fundEnd[t] <- fundEnd[t] + fundAdj[t]
      }
      resultContainer$Proj$PremLoad <- premLoad
      resultContainer$Proj$ExpnsChrg <- expnsChrg
      resultContainer$Proj$IntrCred <- iCred
      resultContainer$Proj$Fund.Adj <- fundAdj
      resultContainer$Proj$Fund <- fundEnd
      return(resultContainer)
   }
)





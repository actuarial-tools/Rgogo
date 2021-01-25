setClass(
   Class = "IPlan.Fund",
   contains = "IPlan",
   slots = c(
      CovPeriod = "numeric",
      PremPeriod = "numeric",
      PremLoadSchd = "numeric",
      ExpnsChrgSchd = "numeric",
      ExpnsChrgMode = "integer",
      ExpnsChrgTiming = "integer",
      ExpnsChrgType = "integer",
      MinIntrCredRate = "numeric",
      SurChrgSchd = "numeric",
      CommSchd = "numeric",
      OvrdOnPremSchd = "numeric",
      OvrdOnCommSchd = "numeric",
      PremTaxRate = "numeric"
   )
)

setValidity(
   Class = "IPlan.Fund",
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
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.InList(valuesAllowed = c(1, 2, 4, 12))
         ), object@ExpnsChrgMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge mode.  It must be 1, 2, 4 or 12."
      }
      # Validate @ExpnsChrgTiming: 0L - beginning of period; 1L: end of period.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.InList(valuesAllowed = c(0, 1))
         ), object@ExpnsChrgTiming
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge mode.  It must be 0 (beginning of period) or 1 (end of period)."
      }
      # Validate @ExpnsChrgType: 0L - dollar amount; 1L: percent of fund balance.
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.InList(valuesAllowed = c(0, 1))
         ), object@ExpnsChrgType
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid expense charge type.  It must be 0 (dollar amount) or 1 (percent of fund balance)."
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

IPlan.Fund <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA, premLoadSchd = numeric(0L),
                       expnsChrgSchd = numeric(0L), expnsChrgMode = 1L, expnsChrgTiming = 0L, expnsChrgType = 0L,
                       surChrgSchd = numeric(0L), minIntrCredRate = numeric(0L),
                       commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                       premTaxRate = 0, id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.Fund",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremLoadSchd = premLoadSchd,
               ExpnsChrgSchd = expnsChrgSchd,
               ExpnsChrgMode = as.integer(expnsChrgMode),
               ExpnsChrgTiming = as.integer(expnsChrgTiming),
               ExpnsChrgType = as.integer(expnsChrgType),
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
   f = "GetCovYears",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@CovPeriod["CovYears"]), Inf, object@CovPeriod["CovYears"])
      years2 <- ifelse(is.na(object@CovPeriod["CovToAge"]), Inf, object@CovPeriod["CovToAge"] - GetIssAge(cov))
      covYears <- min(years1, years2)
      return(covYears)
   }
)

setMethod(
   f = "GetPremYears",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      years1 <- ifelse(is.na(object@PremPeriod["PremYears"]), Inf, object@PremPeriod["PremYears"])
      years2 <- ifelse(is.na(object@PremPeriod["PremToAge"]), Inf, object@PremPeriod["PremToAge"] - GetIssAge(cov))
      premYears <- min(years1, years2)
      return(premYears)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      return(GetRiskClass(cov))
   }
)

setMethod(
   f = "GetPremLoadSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@PremLoadSchd)
      } else if (length(object@PremLoadSchd) == 0) {
         return(rep(0, length.out = GetCovMonths(object, cov)))
      } else if (length(object@PremLoadSchd) == 1) {
         return(rep(object@PremLoadSchd, length.out = GetCovMonths(object, cov)))
      } else {
         return(FillTail(rep(object@PremLoadSchd, each = 12), filler = 0, len = GetCovMonths(object, cov)))
      }
   }
)

setMethod(
   f = "SetPremLoadSchd<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@PremLoadSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@ExpnsChrgSchd)
      }
      if (length(object@ExpnsChrgSchd) == 0) {
         return(rep(0, len = GetCovMonths(object, cov)))
      }
      covMonths <- GetCovMonths(object, cov)
      if (length(object@ExpnsChrgSchd) == 1) {
         expnsChrg <- rep(object@ExpnsChrgSchd, len = covMonths)
      } else {
         expnsChrg <- FillTail(rep(object@ExpnsChrgSchd, each = 12), filler = 0, len = covMonths)
      }
      expnsChrg <- expnsChrg * (((1:covMonths) - 1) %% (12 / GetExpnsChrgMode(object)) == 0)
      return(expnsChrg)
   }
)

setMethod(
   f = "SetExpnsChrgSchd<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@ExpnsChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgMode",
   signature = "IPlan.Fund",
   definition = function(object) {
      return(object@ExpnsChrgMode)
   }
)

setMethod(
   f = "SetExpnsChrgMode<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@ExpnsChrgMode <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgTiming",
   signature = "IPlan.Fund",
   definition = function(object) {
      return(object@ExpnsChrgTiming)
   }
)

setMethod(
   f = "SetExpnsChrgTiming<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@ExpnsChrgTiming <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsChrgType",
   signature = "IPlan.Fund",
   definition = function(object) {
      return(object@ExpnsChrgType)
   }
)

setMethod(
   f = "SetExpnsChrgType<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@ExpnsChrgType <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetSurChrgSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
     if (is.null(cov)) {
         return(object@SurChrgSchd)
      } else if (length(object@SurChrgSchd) == 0) {
         return(rep(0, length.out = GetCovMonths(object, cov)))
      } else if (length(object@SurChrgSchd) == 1) {
         return(rep(object@SurChrgSchd, length.out = GetCovMonths(object, cov)))
      } else {
         return(FillTail(rep(object@SurChrgSchd, each = 12), filler = 0, len = GetCovMonths(object, cov)))
      }
   }
)

setMethod(
   f = "SetSurChrgSchd<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@SurChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMinIntrCredRate",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@MinIntrCredRate)
      } else if (length(object@MinIntrCredRate) == 0) {
         return(rep(-Inf, length.out = GetCovMonths(object, cov)))
      } else if (length(object@MinIntrCredRate) == 1) {
         return(rep(object@MinIntrCredRate, length.out = GetCovMonths(object, cov)))
      } else {
         return(FillTail(rep(object@MinIntrCredRate, each = 12), filler = -Inf, len = GetCovMonths(object, cov)))
      }
   }
)

setMethod(
   f = "SetMinIntrCredRate<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@MinIntrCredRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIntrCredDate",
   signature = "IPlan.Fund",
   definition = function(object, cov) {
      return(GetIssDate(cov) %m+% months(1:GetCovMonths(object, cov)))
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CommSchd)
      }
      schd <- GetCommSchd(cov)
      if (!is.null(schd)) {
         SetCommSchd(object) <- GetValue(schd)
         SetCommSchd(cov) <- Const()
         return(GetCommSchd(object, cov))
      }
      if (length(object@CommSchd) > 0) {
         comm <- FillZeroIfNA(rep(object@CommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         comm <- rep(0, len = GetCovMonths(object, cov))
      }
      return(comm)
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@CommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnCommSchd)
      }
      schd <- GetOvrdOnCommSchd(cov)
      if (!is.null(schd)) {
         SetOvrdOnCommSchd(object) <- GetValue(schd)
         SetOvrdOnCommSchd(cov) <- Const()
         return(GetOvrdOnCommSchd(object, cov))
      }
      if (length(object@OvrdOnCommSchd) > 0) {
         ovrd <- FillZeroIfNA(rep(object@OvrdOnCommSchd, each = 12), GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, len = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@OvrdOnCommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "IPlan.Fund",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnPremSchd)
      }
      schd <- GetOvrdOnPremSchd(cov)
      if (!is.null(schd)) {
         SetOvrdOnPremSchd(object) <- GetValue(schd)
         SetOvrdOnPremSchd(cov) <- Const()
         return(GetOvrdOnPremSchd(object, cov))
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
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@OvrdOnPremSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremTaxRate",
   signature = "IPlan.Fund",
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
   signature = "IPlan.Fund",
   definition = function(object, value) {
      object@PremTaxRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      modPrem <- GetModPrem(cov)
      if (HasValue(modPrem)) {
         prem <- FillTail(rep(c(modPrem, rep(0, times = (12 / GetPremMode(cov) - 1))), length.out = GetPremMonths(object, cov)), filler = 0, len = covMonths)
         # Get actual premium by taking into account premium persistency assumption.
         # If no premium assumption is made, assume planned premium is paid throughout the premium period.
         premAssump <- GetPremAssump(resultContainer)
         if (is.null(premAssump)) {
            premAdj <- rep(1, length.out = covMonths)
         } else {
            premAdj <- GetAssump(premAssump, cov, object, ApplyPremMargin(resultContainer$.ArgSet))
            prem <- prem * premAdj
         }
      } else {
         prem <- rep(0, length.out = covMonths)
      }
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
   signature = "IPlan.Fund",
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
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      resultContainer$Proj$Ben.Dth <- ifelse(resultContainer$Proj$Fund > 0, resultContainer$Proj$Fund, 0)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurChrg",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      surChrg <- resultContainer$Proj$Fund * GetSurChrgSchd(object, cov)
      if (!all(surChrg == 0)) {
         resultContainer$Proj$Chrg.Sur <- surChrg
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      resultContainer <- ProjSurChrg(object, cov, resultContainer)
      if (is.null(resultContainer$Proj$Chrg.Sur)) {
         surBen <- resultContainer$Proj$Fund
      } else {
         surBen <- resultContainer$Proj$Fund - resultContainer$Proj$Chrg.Sur
      }
      resultContainer$Proj$Ben.Sur <- ifelse(surBen > 0, surBen, 0)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      matBen <- ifelse(resultContainer$Proj$Fund[covMonths] > 0, resultContainer$Proj$Fund[covMonths], 0)
      resultContainer$Proj$Ben.Mat <- c(rep(0, length.out = covMonths - 1), matBen)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjFund",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      if (!is.null(resultContainer$Proj$Prem)) {
         prem <- resultContainer$Proj$Prem
      } else {
         prem <- rep(0, length.out = covMonths)
      }
      premLoad <- -prem * GetPremLoadSchd(object, cov)
      expnsChrg <- -GetExpnsChrgSchd(object, cov)

      # Get interest credit rate
      iMin <- GetMinIntrCredRate(object, cov)
      intrCredAssump <- GetIntrCredAssump(resultContainer)
      if (is.null(intrCredAssump)) {
         i <- iMin
      } else {
         i <- GetAssump(intrCredAssump, cov, object)
         i <- ifelse(i < iMin, iMin, i)
      }
      j <- (1 + i) ^ (1 / 12) - 1
      projStartPolMonth <- GetProjPolMonths(resultContainer$Timeline)[1]
      accBal <- GetAccBal(cov)
      fundAdj <- iCred <- fundBeg <- fundEnd <- expnsChrg <- rep(0, covMonths)
      expnsChrgTiming <- GetExpnsChrgTiming(object)
      expnsChrgType <- GetExpnsChrgType(object)
      resultContainer$.ProjEndPolMonth <- covMonths
      for (t in 1:covMonths) {
         fundBeg[t] <- ifelse(t == 1, 0, fundEnd[t - 1])
         fundBeg[t] <- fundBeg[t] + expnsChrg[t] * (expnsChrgTiming == 0) * ifelse(expnsChrgType == 0, 1, fundBeg[t])
         openBal <- fundBeg[t] + prem[t] + premLoad[t]
         iCred[t] <- openBal * j[t]
         fundEnd[t] <- openBal + iCred[t]
         fundEnd[t] <- fundEnd[t] + expnsChrg[t] * (expnsChrgTiming == 1) * ifelse(expnsChrgType == 0L, 1, fundEnd[t])
         fundAdj[t] <- (accBal - fundEnd[t]) * (t == projStartPolMonth)
         fundEnd[t] <- fundEnd[t] + fundAdj[t]
         if (t >=  projStartPolMonth & fundEnd[t] < 0) {
            resultContainer$.ProjEndPolMonth <- t
            break
         }
      }
      resultContainer$Proj$Fund.PremLoad <- premLoad
      resultContainer$Proj$Fund.ExpnsChrg <- expnsChrg
      resultContainer$Proj$Fund.IntrCred <- iCred
      resultContainer$Proj$Fund.Adj <- fundAdj
      resultContainer$Proj$Fund <- fundEnd
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.Fund",
   definition = function(object, cov, resultContainer = list()) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjFund(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- .AdjProjForNegFundBal(object, cov, resultContainer)
      return(resultContainer)
   }
)

.AdjProjForNegFundBal <- function(object, cov, resultContainer) {
   v1 <- (1:GetCovMonths(object, cov)) <= resultContainer$.ProjEndPolMonth    # for policy values arising at end of policy months
   v0 <- ShiftRight(v1, positions = 1, filler = TRUE)   # for policy values arising at beginning of policy months
   projItems <- names(resultContainer$Proj)
   for (itemName in names(resultContainer$Proj)) {
      proj <- eval(parse(text = paste0("resultContainer$Proj$", itemName)))
      if (!is.numeric(proj)) next
      if (itemName %in% c("Ben.Dth", "Ben.Mat", "Ben.Sur", "CV", "Chrg.Sur", "Rein.Ben")) {
         eval(expr = parse(text = paste0("resultContainer$Proj$", itemName, " <- proj * v1")))
      } else {
         eval(expr = parse(text = paste0("resultContainer$Proj$", itemName, " <- proj * v0")))
      }
   }
   return(resultContainer)
}






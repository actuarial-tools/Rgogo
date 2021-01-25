setClass(
   Class = "IPlan.UL",                 # This class implements universal life type B
   contains = c("IPlan.LT", "IPlan.Fund"),
   slots = c(
      PremLoadSchd2 ="numeric",        # Premium load on excess premium.
      CommSchd2 = "numeric",           # Commssion on excess premium (i.e. excess commission).
      OvrdOnPremSchd2 = "numeric",     # Override on excess premium.
      OvrdOnCommSchd2 = "numeric",     # Override on excess commission.
      COITable = "character"           # Cost of insurance table.
   )
)

setValidity(
   Class = "IPlan.UL",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @PremLoadSchd2
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@PremLoadSchd2)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid excess premium load.  Rates must be between 0 and 1."
      }
      # Validate @CommSchd2, @OvrdOnPremSchd2 and @OvrdOnCommSchd2
      vg <- ValidatorGroup(
         Validator.Range(minValue = 0, maxValue = 1, allowNA = FALSE)
      )
      if (Validate(vg, object@CommSchd2) != TRUE) {
         AddMessage(err) <- "Invaid excess commission schedule. Rates must be between 0 and 1."
      }
      if (Validate(vg, object@OvrdOnPremSchd2) != TRUE) {
         AddMessage(err) <- "Invaid override on excess premium schedule. Rates must be between 0 and 1."
      }
      if (Validate(vg, object@OvrdOnCommSchd2) != TRUE) {
         AddMessage(err) <- "Invaid override on excess commission schedule. Rates must be between 0 and 1."
      }
      # Validate @COITable
      if (length(object@COITable) > 1){
         vg <- ValidatorGroup(
            Validator.Names(hasNames = TRUE)
         )
      } else {
         vg <- ValidatorGroup(
            Validator.Names(hasNames = FALSE)
         )
      }
      if (Validate(vg, object@COITable) != TRUE) {
         AddMessage(err) <- "Invalid cost of insurance table name attribute setting."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.UL <- function(covYears = NA, covToAge = NA,
                     premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                     polFee = numeric(0), premTaxRate = numeric(0L), coiTable = character(0L),
                     premLoadSchd = numeric(0L), premLoadSchd2 = numeric(0L),
                     expnsChrgSchd = numeric(0L), expnsChrgMode = 1L, expnsChrgTiming = 0L, expnsChrgType = 0L,
                     surChrgSchd = numeric(0L), minIntrCredRate = numeric(0L),
                     commSchd = numeric(0L), commSchd2 = numeric(0L),
                     ovrdOnPremSchd = numeric(0L), ovrdOnPremSchd2 = numeric(0L),
                     ovrdOnCommSchd = numeric(0L), ovrdOnCommSchd2 = numeric(0L),
                     rein = character(0L), id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.UL",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               COITable = coiTable,
               PremLoadSchd = premLoadSchd,
               PremLoadSchd2 = premLoadSchd2,
               ExpnsChrgSchd = expnsChrgSchd,
               ExpnsChrgMode = as.integer(expnsChrgMode),
               ExpnsChrgTiming = as.integer(expnsChrgTiming),
               ExpnsChrgType = as.integer(expnsChrgType),
               SurChrgSchd = surChrgSchd,
               MinIntrCredRate = minIntrCredRate,
               CommSchd = commSchd,
               CommSchd2 = commSchd2,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnPremSchd2 = ovrdOnPremSchd2,
               OvrdOnCommSchd = ovrdOnCommSchd,
               OvrdOnCommSchd2 = ovrdOnCommSchd2,
               Rein = rein,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}


setMethod(
   f = "GetPremLoadSchd2",
   signature = "IPlan.UL",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@PremLoadSchd2)
      } else if (length(object@PremLoadSchd2) == 0) {
         return(rep(0, length.out = GetCovMonths(object, cov)))
      } else if (length(object@PremLoadSchd2) == 1) {
         return(rep(object@PremLoadSchd2, length.out = GetCovMonths(object, cov)))
      } else {
         return(FillTail(rep(object@PremLoadSchd2, each = 12), filler = 0, len = GetCovMonths(object, cov)))
      }
   }
)

setMethod(
   f = "SetPremLoadSchd2<-",
   signature = "IPlan.UL",
   definition = function(object, value) {
      object@PremLoadSchd2 <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCommSchd2",
   signature = "IPlan.UL",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CommSchd2)
      }
      schd <- GetCommSchd2(cov)
      if (!is.null(schd)) {
         SetCommSchd2(object) <- GetValue(schd)
         SetCommSchd2(cov) <- Const()
         return(GetCommSchd2(object, cov))
      }
      if (HasValue(object@CommSchd2)) {
         comm <- FillTail(rep(object@CommSchd2, each = 12), filler = 0, len = GetCovMonths(object, cov))
      } else {
         comm <- rep(0, length.out = GetCovMonths(object, cov))
      }
      return(comm)
   }
)

setMethod(
   f = "SetCommSchd2<-",
   signature = "IPlan.UL",
   definition = function(object, value) {
      object@CommSchd2 <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd2",
   signature = "IPlan.UL",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnCommSchd2)
      }
      schd <- GetOvrdOnCommSchd2(cov)
      if (!is.null(schd)) {
         SetOvrdOnCommSchd2(object) <- GetValue(schd)
         SetOvrdOnCommSchd2(cov) <- Const()
         return(GetOvrdOnCommSchd2(object, cov))
      }
      if (HasValue(object@OvrdOnCommSchd2)) {
         ovrd <- FillTail(rep(object@OvrdOnCommSchd2, each = 12), filler = 0, len = GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, length.out = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnCommSchd2<-",
   signature = "IPlan.UL",
   definition = function(object, value) {
      object@OvrdOnCommSchd2 <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd2",
   signature = "IPlan.UL",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@OvrdOnPremSchd2)
      }
      schd <- GetOvrdOnPremSchd2(cov)
      if (!is.null(schd)) {
         SetOvrdOnPremSchd2(object) <- GetValue(schd)
         SetOvrdOnPremSchd2(cov) <- Const()
         return(GetOvrdOnPremSchd2(object, cov))
      }
      if (HasValue(object@OvrdOnPremSchd2)) {
         ovrd <- FillTail(rep(object@OvrdOnPremSchd2, each = 12), filler = 0, len = GetCovMonths(object, cov))
      } else {
         ovrd <- rep(0, length.out = GetCovMonths(object, cov))
      }
      return(ovrd)
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = "IPlan.UL",
   definition = function(object, value) {
      object@OvrdOnPremSchd2 <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCOITable",
   signature = "IPlan.UL",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@COITable)
      }
      if (length(object@COITable) == 0) {
         return(NULL)
      }
      if (length(object@COITable) == 1) {
         tblId <- object@COITable
      } else {
         riskClass <- GetRiskClass(object, cov)
         tblId <- object@COITable[riskClass]
      }
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "SetCOITable<-",
   signature = "IPlan.UL",
   definition = function(object, value) {
      object@COITable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      premMonths <- GetPremMonths(object, cov)
      # Get planned premium.  If cov contains modal premium information, modal premium is used for planned premium.
      # Otherwise, calculate minimum premium from premium table, and set it as planned premium.
      modMinPrem <- GetModPrem(object, cov)
      modPrem <- GetModPrem(cov)
      if (!HasValue(modPrem)) {
         modPrem <- modMinPrem
      }
      # Get actual premium by taking into account premium persistency assumption.
      # If no premium assumption is made, assume planned premium is paid throughout the premium period.
      premAssump <- GetPremAssump(resultContainer)
      if (is.null(premAssump)) {
         premAdj <- rep(1, length.out = covMonths)
      } else {
         premAdj <- GetAssump(premAssump, cov, object, ApplyPremMargin(resultContainer$.ArgSet))
      }
      isPayable <- ((((1:covMonths) - 1) %% (12 / premMode) == 0) * ((1:covMonths) <= premMonths))
      projPrem <- modPrem * premAdj * isPayable
      projMinPrem <- modMinPrem * isPayable
      # Determine projected minimum premium and excess premium
      resultContainer$Proj$Prem <- projPrem
      resultContainer$Proj$Prem.Min <- pmin(projPrem, projMinPrem)
      resultContainer$Proj$Prem.Exs <- projPrem - resultContainer$Proj$Prem.Min
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer) {
      # Commission
      resultContainer$Proj$Comm.Min <- GetCommSchd(object, cov) * resultContainer$Proj$Prem.Min
      resultContainer$Proj$Comm.Exs <- GetCommSchd2(object, cov) * resultContainer$Proj$Prem.Exs
      resultContainer$Proj$Comm <- resultContainer$Proj$Comm.Min + resultContainer$Proj$Comm.Exs
      # Override on commission
      ovrdOnMinPrem <- GetOvrdOnPremSchd(object, cov) * resultContainer$Proj$Prem.Min
      ovrdOnExsPrem <- GetOvrdOnPremSchd2(object, cov) * resultContainer$Proj$Prem.Exs
      ovrdOnMinComm <- GetOvrdOnCommSchd(object, cov) * resultContainer$Proj$Comm.Min
      ovrdOnExsComm <- GetOvrdOnCommSchd2(object, cov) * resultContainer$Proj$Comm.Exs
      resultContainer$Proj$Comm.Ovrd.Min <- ovrdOnMinPrem + ovrdOnMinComm
      resultContainer$Proj$Comm.Ovrd.Exs <- ovrdOnExsPrem + ovrdOnExsComm
      resultContainer$Proj$Comm.Ovrd <- resultContainer$Proj$Comm.Ovrd.Min + resultContainer$Comm.Ovrd.Exs
      return(resultContainer)
   }
)

setMethod(
   f = "ProjFund",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      # Project premium load
      minPremLoad <- -resultContainer$Proj$Prem.Min * GetPremLoadSchd(object, cov)
      exsPremLoad <- -resultContainer$Proj$Prem.Exs * GetPremLoadSchd2(object, cov)
      premLoad <- minPremLoad + exsPremLoad
      prem <- resultContainer$Proj$Prem
      # Project expense charge
      expnsChrg <- -GetExpnsChrgSchd(object, cov)
      # Get cost of insurance charge rate
      coiTable <- GetCOITable(object, cov)
      if (!is.null(coiTable)) {
         coiRate <- rep(LookUp(coiTable, cov), each = 12) / 12     # Monthly cost of insurance charge rate
         length(coiRate) <- covMonths
      } else {
         coiRate <- rep(0, length.out = covMonths)
      }
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
      projStartPolMonth <- GetProjPolMonths(result$Timeline)[1]
      # Project fund
      accBal <- GetAccBal(cov)
      fundAdj <- iCred <- fundBeg <- fundEnd <- expnsChrg <- coi <- naar <- rep(0, covMonths)
      expnsChrgTiming <- GetExpnsChrgTiming(object)
      expnsChrgType <- GetExpnsChrgType(object)
      resultContainer$.ProjEndPolMonth <- covMonths
      for (t in 1:covMonths) {
         fundBeg[t] <- ifelse(t == 1, 0, fundEnd[t - 1])
         fundBeg[t] <- fundBeg[t] + expnsChrg[t] * (expnsChrgTiming == 0) * ifelse(expnsChrgType == 0, 1, fundBeg[t])
         openBal <- fundBeg[t] + prem[t] + premLoad[t]
         # Calculate net amount at risk and cost of insurance
         naar[t] <- GetFaceAmt(cov)     # Net amount at risk of type B UL is equal to face amount.
         coi[t] <- -naar[t] * coiRate[t]
         openBal <- fundBeg[t] + prem[t] + premLoad[t] + expnsChrg[t] * (expnsChrgTiming == 0) + coi[t]
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
      # Save fund projection results
      resultContainer$Proj$Fund.PremLoad.MinPrem <- minPremLoad
      resultContainer$Proj$Fund.PremLoad.ExsPrem <- exsPremLoad
      resultContainer$Proj$Fund.PremLoad <- premLoad
      resultContainer$Proj$Fund.ExpnsChrg <- expnsChrg
      resultContainer$Proj$Fund.IntrCred <- iCred
      resultContainer$Proj$Fund.Adj <- fundAdj
      resultContainer$Proj$Fund <- fundEnd
      resultContainer$Proj$CV <- fundEnd
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer){
      resultContainer$Proj$Ben.Dth <- GetFaceAmt(cov) + ShiftRight(resultContainer$Proj$Fund, positions = 1, filler = 0)
      return(resultContainer)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer){
      return(ProjMatBen(as(object, "IPlan.Fund"), cov, resultContainer))
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.UL",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjFund(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resutlContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      resultContainer <- .AdjProjForNegFundBal(object, cov, resultContainer)
      return(resultContainer)
   }
)



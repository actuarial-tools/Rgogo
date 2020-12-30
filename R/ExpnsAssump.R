#' @include IExpnsAssump.R
NULL

setClass(
   Class = "ExpnsAssump",
   contains = "IExpnsAssump",
   slots = c(
      MEPerPol = "numeric",
      MEPerPolInflRate = "numeric",
      MEPerPolMargin = "numeric",
      MEPerPrem = "numeric",
      MEPerPremInflRate = "numeric",
      MEPerPremMargin = "numeric",
      MEPerPremAmt = "numeric",
      MEPerPremAmtMargin = "numeric",
      AEPerPol = "numeric",
      AEPerPolMargin = "numeric",
      AEPerFaceAmt = "numeric",
      AEPerFaceAmtMargin = "numeric"
   )
)

setValidity(
   Class = "ExpnsAssump",
   method = function(object) {
      err <- New.SysMessage()
      v <- Validator.Length(minLen = 1, maxLen = 1)
      # Validate @MEPerPol
      isValid <- Validate(v, object@MEPerPol)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPol in a object of or extending class 'ExpnsAssump' must contain a numeric value of length 1."
      }
      # Validate @MEPerPrem
      isValid <- Validate(v, object@MEPerPrem)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPrem in a object of or extending class 'ExpnsAssump' must contain a numeric value of length 1."
      }
      # Validate @MEPerPremAmt
      isValid <- Validate(v, object@MEPerPremAmt)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPremAmt in a object of or extending class 'ExpnsAssump' must contain a numeric value of length 1."
      }
      # Validate @AEPerPol
      if (length(object@AEPerPol) > 1) {
         v <- Validator.Names(hasNames = TRUE, namesAllowed = as.character(0:100))
      } else {
         v <- Validator.Names(hasNames = FALSE)
      }
      isValid <- Validate(v, object@AEPerPol)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@AEPerPol in a object of or extending class 'ExpnsAssump' contains a value with invalid name attributes."
      }
      # Validate @AEPerFaceAmt
      v <- Validator.Length(minLen = 1, maxLen = 1)
      isValid <- Validate(v, object@AEPerFaceAmt)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@AEPerFaceAmt in a object of or extending class 'ExpnsAssump' must contain a numeric value of length 1."
      }
      v <- Validator.Length(minLen = 1, maxLen = 9999)
      # Validate @MEPerPolInflRate
      isValid <- Validate(v, object@MEPerPolInflRate)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPolInflRate in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @MEPerPremInflRate
      isValid <- Validate(v, object@MEPerPremInflRate)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPremInflRate in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @MEPerPolMargin
      isValid <- Validate(v, object@MEPerPolMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPolMargin in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @MEPerPremMargin
      isValid <- Validate(v, object@MEPerPremMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPremMargin in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @MEPerPremAmtMargin
      isValid <- Validate(v, object@MEPerPremAmtMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@MEPerPremAmtMargin in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @AEPerPolMargin
      isValid <- Validate(v, object@AEPerPolMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@AEPerPolMargin in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      # Validate @AEPerFaceAmtMargin
      isValid <- Validate(v, object@AEPerFaceAmtMargin)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@AEPerFaceAmtMargin in a object of or extending class 'ExpnsAssump' must contain a numeric value of length at least 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

ExpnsAssump <- function(mePerPol = 0, mePerPolInflRate = 0, mePerPolMargin = 0,
                        mePerPrem = 0, mePerPremInflRate = 0, mePerPremMargin = 0,
                        mePrePremAmt = 0, mePerPremAmtMargin = 0,
                        aePerPol = 0, aePerPolMargin = 0,
                        aePerFaceAmt = 0, aePerFaceAmtMargin = 0,
                        id = character(0L), descrip = character(0L)) {
   assump <- new(
      Class = "ExpnsAssump",
      Id = as.character(id),
      MEPerPol = mePerPol,
      MEPerPolInflRate = mePerPolInflRate,
      MEPerPolMargin = mePerPolMargin,
      MEPerPrem = mePerPrem,
      MEPerPremInflRate = mePerPremInflRate,
      MEPerPremMargin = mePerPremMargin,
      MEPerPremAmt = mePrePremAmt,
      MEPerPremAmtMargin = mePerPremAmtMargin,
      AEPerPol = aePerPol,
      AEPerPolMargin = aePerPolMargin,
      AEPerFaceAmt = aePerFaceAmt,
      AEPerFaceAmtMargin = aePerFaceAmtMargin,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}

setMethod(
   f = "ProjMntExpnsPerPol",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      tInfo <- assumpInfo$Timeline
      # expns <- c(rep(object@MEPerPol / 12, length.out = GetProjLen(tInfo) - 1), 0)
      projLen <- GetProjLen(tInfo)
      expns <- rep(object@MEPerPol / 12, length.out = projLen)
      inflAdj <- .GetInflAdjVector(object@MEPerPolInflRate, projLen)
      me.PerPol.Expd <- expns * inflAdj * (GetProjTimeline(tInfo) >= GetIssDate(cov))
      me.PerPol.Expd <- me.PerPol.Expd * GetExpnsWeight(cov)
      names(me.PerPol.Expd) <- NULL
      assumpInfo$me.PerPol.Expd <- me.PerPol.Expd
      return(assumpInfo)
   }
)

setMethod(
   f = "ProjMntExpnsPerPrem",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      tInfo <- assumpInfo$Timeline
      projLen <- GetProjLen(tInfo)
      if (!is.null(assumpInfo$Proj$Prem)) {
         # prem <- c(rep(0, length.out = projLen - GetCovProjLen(tInfo)), assumpInfo$Proj$Prem[GetCovProjTimeIndex(tInfo) >= 0])
         prem <- c(rep(0, length.out = projLen - GetCovProjLen(tInfo)), assumpInfo$Proj$Prem[GetProjPolMonths(tInfo)])
         expns <- rep(object@MEPerPrem, length.out = projLen)
         inflAdj <- .GetInflAdjVector(object@MEPerPremInflRate, projLen)
         me.PerPrem.Expd <- expns * inflAdj * (prem != 0)
         me.PerPremAmt.Expd <- prem * object@MEPerPremAmt
      } else {
         me.PerPrem.Expd <- rep(0, length.out = projLen)
         me.PerPremAmt.Expd <- rep(0, length.out = projLen)
      }
      me.PerPrem.Expd <- me.PerPrem.Expd * GetExpnsWeight(cov)
      names(me.PerPrem.Expd) <- NULL
      assumpInfo$me.PerPrem.Expd <- me.PerPrem.Expd
      names(me.PerPremAmt.Expd) <- NULL
      assumpInfo$me.PerPremAmt.Expd <- me.PerPremAmt.Expd
      return(assumpInfo)
   }
)

setMethod(
   f = "ProjAcqExpns",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      projLen <- GetProjLen(assumpInfo$Timeline)
      covProjLen <- GetCovProjLen(assumpInfo$Timeline)
      if (projStartDate <= GetIssDate(cov)) {
         if (length(object@AEPerPol) == 1) {
            ae.PerPol.Expd <- FillZeroIfNA(object@AEPerPol, len = covProjLen)
         } else {
            ae.PerPol.Expd <- FillZeroIfNA(object@AEPerPol[as.character(GetIssAge(cov))], len = covProjLen)
         }
         ae.PerPol.Expd <- c(rep(0, length.out = projLen - covProjLen), ae.PerPol.Expd)
         ae.PerFaceAmt.Expd <- c(rep(0, length.out = projLen - covProjLen),
                                 FillZeroIfNA(GetFaceAmt(cov) * object@AEPerFaceAmt, len = covProjLen))
      } else {
         ae.PerPol.Expd <- rep(0, length.out = projLen)
         ae.PerFaceAmt.Expd <- rep(0, length.out = projLen)
      }
      ae.PerPol.Expd <- ae.PerPol.Expd * GetExpnsWeight(cov)
      names(ae.PerPol.Expd) <- NULL
      assumpInfo$ae.PerPol.Expd <- ae.PerPol.Expd
      names(ae.PerFaceAmt.Expd) <- NULL
      assumpInfo$ae.PerFaceAmt.Expd <- ae.PerFaceAmt.Expd
      return(assumpInfo)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      stopifnot(projStartDate <= GetExpiryDate(plan, cov))
      assumpInfo <- ProjMntExpnsPerPol(object, cov, plan, assumpInfo, projStartDate)
      assumpInfo <- ProjMntExpnsPerPrem(object, cov, plan, assumpInfo, projStartDate)
      # assumpInfo <- ProjMntExpnsPerClaim(object, cov, plan, assumpInfo, projStartDate)
      assumpInfo <- ProjAcqExpns(object, cov, plan, assumpInfo, projStartDate)
      return(assumpInfo)
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      stopifnot(projStartDate <= GetExpiryDate(plan, cov))
      projLen <- length(assumpInfo$me.PerPol.Expd)
      assumpInfo$me.PerPol.Padd <- assumpInfo$me.PerPol.Expd * (1 + .GetExpnsMargin(object@MEPerPolMargin, projLen))
      assumpInfo$me.PerPrem.Padd <- assumpInfo$me.PerPrem.Expd * (1 + .GetExpnsMargin(object@MEPerPremMargin, projLen))
      assumpInfo$me.PerPremAmt.Padd <- assumpInfo$me.PerPremAmt.Expd * (1 + .GetExpnsMargin(object@MEPerPremAmtMargin, projLen))
      # assumpInfo$me.PerClaim.Padd <- assumpInfo$me.PerClaim.Expd * (1 + .GetExpnsMargin(object@MEPerClaimMargin, projLen))
      assumpInfo$ae.PerPol.Padd <- assumpInfo$ae.PerPol.Expd * (1 + .GetExpnsMargin(object@AEPerPolMargin, projLen))
      assumpInfo$ae.PerFaceAmt.Padd <- assumpInfo$ae.PerFaceAmt.Expd * (1 + .GetExpnsMargin(object@AEPerFaceAmtMargin, projLen))
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "ExpnsAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      if (is.null(projStartDate)) {
         projStartDate <- GetIssDate(cov)
      }
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo, projStartDate)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo, projStartDate)
      return(assumpInfo)

   }
)

setMethod(
   f = "GetMntExpnsPerPol",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPol)
   }
)

setMethod(
   f = "SetMntExpnsPerPol<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPol <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPrem",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPrem)
   }
)

setMethod(
   f = "SetMntExpnsPerPrem<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPrem <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetMntExpnsPerPremAmt",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPremAmt)
   }
)

setMethod(
   f = "SetMntExpnsPerPremAmt<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPremAmt <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAcqExpnsPerPol",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@AEPerPol)
   }
)

setMethod(
   f = "SetAcqExpnsPerPol<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@AEPerPol <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAcqExpnsPerFaceAmt",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@AEPerFaceAmt)
   }
)

setMethod(
   f = "SetAcqExpnsPerFaceAmt<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@AEPerFaceAmt <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPolInflRate",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPolInflRate)
   }
)

setMethod(
   f = "SetMntExpnsPerPolInflRate<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPolInflRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPremInflRate",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPremInflRate)
   }
)

setMethod(
   f = "SetMntExpnsPerPremInflRate<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPremInflRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPolMargin",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPolMargin)
   }
)

setMethod(
   f = "SetMntExpnsPerPolMargin<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPolMargin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPremMargin",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPremMargin)
   }
)

setMethod(
   f = "SetMntExpnsPerPremMargin<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPremMargin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMntExpnsPerPremAmtMargin",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@MEPerPremAmtMargin)
   }
)

setMethod(
   f = "SetMntExpnsPerPremAmtMargin<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@MEPerPremAmtMargin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAcqExpnsPerPolMargin",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@AEPerPolMargin)
   }
)

setMethod(
   f = "SetAcqExpnsPerPolMargin<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@AEPerPolMargin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAcqExpnsPerFaceAmtMargin",
   signature = "ExpnsAssump",
   definition = function(object) {
      return(object@AEPerFaceAmtMargin)
   }
)

setMethod(
   f = "SetAcqExpnsPerFaceAmtMargin<-",
   signature = "ExpnsAssump",
   definition = function(object, value) {
      object@AEPerFaceAmtMargin <- value
      validObject(object)
      return(object)
   }
)

.GetInflAdjVector <- function(inflSlotValue, projLen) {
   l <- ceiling(projLen / 12)   # Number of projection years
   if (length(inflSlotValue) == 1) {
      inflRate <- c(0, RepeatTail(inflSlotValue, len = l - 1))
   } else {
      inflRate <- c(0, FillZeroIfNA(inflSlotValue, len = l - 1))
   }
   inflRate <- rep(inflRate, each = 12, length.out = projLen) * (((1:projLen) - 1) %% 12 == 0)
   inflAdj <- cumprod(1 + inflRate)
   return(inflAdj)
}

.GetExpnsMargin <- function(marginSlotValue, projLen) {
   if (length(marginSlotValue) == 1) {
      return(marginSlotValue)
   } else {
      return(FillZeroIfNA(marginSlotValue, len = projLen))
   }
}





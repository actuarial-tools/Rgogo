setClass(
   Class = "Rein",
   contains = "IRein",
   slots = c(
      PremTable = "character",
      PremTableMult = "numeric",
      CommSchd = "numeric",
      PremMode = "integer",
      RetnProp = "numeric",
      RetnLimit = "numeric",
      MinReinAmt = "numeric",
      RfndUrndPremOnLapse = "logical",
      RfndUrndCommOnLapse = "logical"
   )
)

setValidity(
   Class = "Rein",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@PremTable) > 1 & is.null(names(object@PremTable))) {
         AddMessage(err) <- "PremTable: the slot value must be a character vector with name attributes."
      }
      if (length(object@PremTableMult) > 1 & is.null(names(object@PremTableMult))) {
         AddMessage(err) <- "PremTableMult: the slot value must be a character vector with name attributes."
      }
      # PremMode: length cannot be greater than 1.
      if (length(object@PremMode) > 1) {
         AddMessage(err) <- "PremMode: the length of the slot value cannot be greater than 1."
      }
      # RetnProp: length cannot be greater than 1.
      if (length(object@RetnProp) > 1) {
         AddMessage(err) <- "RetnProp: the length of the slot value cannot be greater than 1."
      }
      # RetnLimit: length cannot be greater than 1.
      if (length(object@RetnLimit) > 1) {
         AddMessage(err) <- "RetnLimit: the length of the slot value cannot be greater than 1."
      }
      # MinReinAmt: length cannot be greater than 1.
      if (length(object@MinReinAmt) > 1) {
         AddMessage(err) <- "MinReinAmt: the length of the slot value cannot be greater than 1."
      }
      # RfndUrndPremOnLapse: length cannot be greater than 1.
      if (length(object@RfndUrndPremOnLapse) > 1) {
         AddMessage(err) <- "RfndUrndPremOnLapse: the length of the slot value cannot be greater than 1."
      }
      # RfndUrndCommOnLapse: length cannot be greater than 1.
      if (length(object@RfndUrndCommOnLapse) > 1) {
         AddMessage(err) <- "RfndUrndCommOnLapse: the length of the slot value cannot be greater than 1."
      }
      if(NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Rein <- function(retnProp, retnLimit, minReinAmt = 0,
                 premTable = character(0L), premTableMult = 1, premMode,
                 commSchd = numeric(0L),rfndUrndPremOnLapse, rfndUrndCommOnLapse,
                 id = character(0L), descrip = character(0L)) {
   object <- new(
      Class = "Rein",
      RetnProp = retnProp,
      RetnLimit = retnLimit,
      MinReinAmt = minReinAmt,
      PremTable = premTable,
      PremTableMult = premTableMult,
      PremMode = premMode,
      CommSchd = commSchd,
      RfndUrndPremOnLapse = rfndUrndPremOnLapse,
      RfndUrndCommOnLapse = rfndUrndCommOnLapse,
      Descrip = as.character(descrip)
   )
   SetId(object) <- as.character(id)
   return (object)
}

setMethod(
   f = "GetPremTable",
   signature = "Rein",
   definition = function(object, cov) {
      if (length(object@PremTable) == 0) {
         return(NULL)
      }
      if (length(object@PremTable) == 1) {
         tblId <- object@PremTable
      } else {
         tblId <- object@PremTable[GetRiskClass(object, cov)]
      }
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "GetPremTableMult",
   signature = "Rein",
   definition = function(object, cov) {
      if (length(object@PremTableMult) > 1) {
         return(object@PremTableMult[GetRiskClass(object, cov)])
      } else {
         return(object@PremTableMult)
      }
   }
)

setMethod(
   f = "GetCommSchd",
   signature = "Rein",
   definition = function(object, cov) {
      if (HasValue(object@CommSchd)) {
         comm <- FillZeroIfNA(rep(object@CommSchd, each = 12), GetCovMonths(cov))
      } else {
         comm <- rep(0, len = GetCovMonths(cov))
      }
      return(comm)
   }
)

setMethod(
   f = "GetPremMode",
   signature = "Rein",
   definition = function(object) {
      return(object@PremMode)
   }
)

setMethod(
   f = "GetRetnProp",
   signature = "IRein",
   definition = function(object) {
      return(object@RetnProp)
   }
)

setMethod(
   f = "GetRetnLimit",
   signature = "IRein",
   definition = function(object) {
      return(object@RetnLimit)
   }
)

setMethod(
   f = "GetMinReinAmt",
   signature = "IRein",
   definition = function(object) {
      return(object@MinReinAmt)
   }
)

setMethod(
   f = "GetRfndUrndPremOnLapse",
   signature = "IRein",
   definition = function(object) {
      return(object@RfndUrndPremOnLapse)
   }
)

setMethod(
   f = "GetRfndUrndCommOnLapse",
   signature = "IRein",
   definition = function(object) {
      return(object@RfndUrndCommOnLapse)
   }
)

setMethod(
   f = "SetPremTable<-",
   signature = "Rein",
   definition = function(object, value) {
      object@PremTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPremTableMult<-",
   signature = "Rein",
   definition = function(object, value) {
      object@PremTableMult <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = "Rein",
   definition = function(object, value) {
      object@CommSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPremMode<-",
   signature = "Rein",
   definition = function(object, value) {
      object@PremMode <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetRetnProp<-",
   signature = "Rein",
   definition = function(object, value) {
      object@RetnProp <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetRetnLimit<-",
   signature = "Rein",
   definition = function(object, value) {
      object@RetnLimit <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetMinReinAmt<-",
   signature = "Rein",
   definition = function(object, value) {
      object@MinReinAmt <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetRfndUrndPremOnLapse<-",
   signature = "Rein",
   definition = function(object, value) {
      object@RfndUrndPremOnLapse <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetRfndUrndCommOnLapse<-",
   signature = "Rein",
   definition = function(object, value) {
      object@RfndUrndCommOnLapse <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "IRein",
   definition = function(object, cov) {
      # Reinsurance risk class is assumed to be the same as coverage risk class by default.
      return(GetRiskClass(cov))
   }
)

setMethod(
   f = "ProjNaar",
   signature = "Rein",
   definition = function(object, cov, resultContainer) {
      if(is.null(resultContainer$Proj$CV)){
         cv <- 0
      } else {
         cv <- ShiftRight(resultContainer$Proj$CV, positions = 1, filler = 0)
      }
      resultContainer$Proj$Naar = resultContainer$Proj$Ben.Dth - cv
      return(resultContainer)
   }
)

setMethod(
   f = "ProjReinNaar",
   signature = "Rein",
   definition = function(object, cov, resultContainer) {
      # Determine reinsured proportion
      if (!HasValue(reinProp <- GetReinProp(cov))) {
         faceAmt <- GetFaceAmt(cov)
         cedAmt <- faceAmt - min(faceAmt * GetRetnProp(object), GetRetnLimit(object))
         reinProp <- cedAmt * (cedAmt >= GetMinReinAmt(object)) / faceAmt
      }
      # Project retention and reinsured Naar
      resultContainer <- ProjNaar(object, cov, resultContainer)
      naar <- resultContainer$Proj$Naar
      resultContainer$Proj$Rein.Naar = naar * reinProp
      resultContainer$Proj$Rein.Retn = naar * (1 - reinProp)
      resultContainer$Proj$Rein.Ben = naar * reinProp
      resultContainer$.ReinProp <- reinProp
      return(resultContainer)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "Rein",
   definition = function(object, cov, resultContainer) {
      reinNaar <- resultContainer$Proj$Rein.Naar
      if (!all(reinNaar == 0)) {
         covMonths <- GetCovMonths(cov)
         reinPremTable <- GetPremTable(object, cov)
         if (!is.null(reinPremTable)) {
            tableRate <- LookUp(reinPremTable, cov)
         } else {
            tableRate <- rep(0, ceiling(covMonths / 12))
         }
         # Project reinsurance premium
         reinPremTableMult <- GetPremTableMult(object, cov)
         reinPremMode <- GetPremMode(object)
         vReinPremRate <- rep((tableRate * reinPremTableMult), each = 12, length.out = covMonths)
         vReinPremPayable <- rep(ifelse((1:12-1) %% (12/reinPremMode) == 0, 1, 0) / reinPremMode, length.out = covMonths)
         projReinPrem <- vReinPremRate * vReinPremPayable * reinNaar
         if(GetRfndUrndPremOnLapse(object)) {
            pctRfnd <- rep(seq(from=1,to=reinPremMode/12,length=(12/reinPremMode)),length=covMonths) * ((1:covMonths - 1) %% (12/reinPremMode) != 0)
            projReinPremRfnd <- projReinPrem[((1:covMonths)-1) %/% 12 * 12 + 1] * pctRfnd
            resultContainer$.pctRfnd <- pctRfnd
         } else {
            projReinPremRfnd <- rep(0, length.out = covMonths)
         }
         resultContainer$Proj$Rein.Prem <- projReinPrem
         resultContainer$Proj$Rein.Prem.Rfnd <- projReinPremRfnd
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjComm",
   signature = "Rein",
   definition = function(object, cov, resultContainer) {
      if (resultContainer$.ReinProp != 0) {
         covMonths <- GetCovMonths(cov)
         reinCommSchd <- GetCommSchd(object, cov)
         projReinPrem <- resultContainer$Proj$Rein.Prem
         projReinComm <- projReinPrem * reinCommSchd
         # Refund unearned reinsurance commission if applicable
         if (GetRfndUrndCommOnLapse(object)) {
            projReinCommRfnd <- projReinComm[((1:covMonths)-1) %/% 12 * 12 + 1] * resultContainer$.pctRfnd
         } else {
            projReinCommRfnd <- rep(0, length.out = covMonths)
         }
         resultContainer$Proj$Rein.Comm <- projReinComm
         resultContainer$Proj$Rein.Comm.Rfnd <- projReinCommRfnd
         resultContainer$.pctRfnd <- NULL
      }
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "Rein",
   definition = function(object, cov, resultContainer, ...) {
      resultContainer <- ProjReinNaar(object, cov, resultContainer)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      return(resultContainer)
   }
)




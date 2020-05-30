#' @include ILapseAssump.R
NULL


setClass(
   Class = "LapseAssump",
   contains = "ILapseAssump",
   slots = c(
      LapseRate = "character_or_numeric",
      LapseRateMult = "numeric",
      LapseRateFlatExtra = "numeric",
      LapsePfad = "numeric"
   )
)


setValidity(
   Class = "LapseAssump",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @LapseRate
      if (is.numeric(object@LapseRate)) {
         v <- Validator.Range(minValue = 0, maxValue = 1)
         if (Validate(v, object@LapseRate) != TRUE) {
            AddMessage(err) <- "Value of slot @LapseRate in class LapseAssump must be between 0 and 1."
         }
      }
      if (is.character(object@LapseRate)) {
         v <- Validator.Names(hasNames = (length(object@LapseRate) > 1))
         if (Validate(v, object@LapseRate) != TRUE) {
            AddMessage(err) <- "Value of slot @LapseRate in class LapseAssump must have name attribute."
         }
      }
      # Validate @LapseRateMult
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@LapseRateMult) > 1)),
         Validator.Range(minValue = 0)
      )
      if (Validate(v, object@LapseRateMult) != TRUE) {
         AddMessage(err) <- "Value of slot @LapseRateMult in class LapseAssump is invalid."
      }
      # Validate @LapseRateFlatExtra
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@LapseRateFlatExtra) > 1)),
         Validator.Range(minValue = -1, maxValue = 1)
      )
      if (Validate(v, object@LapseRateFlatExtra) != TRUE) {
         AddMessage(err) <- "Value of slot @LapseRateFlatExtra in class LapseAssump must be between -1 and 1.  In addition, it must have name attributes if the length is greater than 1."
      }
      # Validate @LapsePfad
      v <- Validator.Names(hasNames = (length(object@LapsePfad) > 1))
      if (Validate(v, object@LapsePfad) != TRUE) {
         AddMessage(err) <- "Value of slot @LapsePfad in class LapseAssump is invalid."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


LapseAssump <- function(id = character()) {
   assump <- new(Class = "LapseAssump",
                 Id = as.character(id),
                 LapseRateMult = 1,
                 LapseRateFlatExtra = 0,
                 LapsePfad = 0)
   return(assump)
}


setMethod(
   f = "GetLapseRate",
   signature = "LapseAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@LapseRate)
      }
      len <- ceiling(GetCovYears(plan, cov))
      if (!HasValue(object@LapseRate)) {
         return(rep(0, len))
      } else if (is.numeric(object@LapseRate)) {
         return(FillTail(object@LapseRate, 0, len))
      } else if (is.character(object@LapseRate)) {
         if (length(object@LapseRate) == 1) {
            tblId <- object@LapseRate
         } else {
            riskClass <- GetRiskClass(object, cov, plan)
            tblId <- object@LapseRate[riskClass]
         }
         tblId <- ifelse(startsWith(tblId, "Lapse."), tblId, paste0("Lapse.", tblId))
         tbl <- eval(expr = parse(text = tblId))
         return(LookUp(tbl, cov))
      }
   }
)


setMethod(
   f = "SetLapseRate<-",
   signature = "LapseAssump",
   definition = function(object, value) {
      object@LapseRate <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetLapseRateMult",
   signature = "LapseAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@LapseRateMult)
      }
      if (length(object@LapseRateMult) == 0) {
         return(1)
      } else if (length(object@LapseRateMult) == 1) {
         return(object@LapseRateMult)
      } else {
         return(object@LapseRateMult[GetRiskClass(object, cov, plan)])
      }
   }
)


setMethod(
   f = "SetLapseRateMult<-",
   signature = "LapseAssump",
   definition = function(object, value) {
      object@LapseRateMult <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetLapseRateFlatExtra",
   signature = "LapseAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@LapseRateFlatExtra)
      }
      if (length(object@LapseRateFlatExtra) == 0) {
         return(0)
      } else if (length(object@LapseRateFlatExtra) == 1) {
         return(object@LapseRateFlatExtra)
      } else {
         return(object@LapseRateFlatExtra[GetRiskClass(object, cov, plan)])
      }
   }
)


setMethod(
   f = "SetLapseRateFlatExtra<-",
   signature = "LapseAssump",
   definition = function(object, value) {
      object@LapseRateFlatExtra <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetLapsePfad",
   signature = "LapseAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if (is.null(cov) & is.null(plan)) {
         return(object@LapsePfad)
      }
      if (length(object@LapsePfad) == 0) {
         return(0)
      } else if (length(object@LapsePfad) == 1) {
         return(object@LapsePfad)
      } else {
         return(object@LapsePfad[GetRiskClass(object, cov, plan)])
      }
   }
)


setMethod(
   f = "SetLapsePfad<-",
   signature = "LapseAssump",
   definition = function(object, value) {
      object@LapsePfad <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetRiskClass",
   signature = "LapseAssump",
   definition = function(object, cov, plan) {
      return(GetRiskClass(plan, cov))
   }
)


setMethod(
   f = "GetExpdAssump",
   signature = "LapseAssump",
   definition = function(object, cov, plan, assumpInfo) {
      w <- GetLapseRate(object, cov, plan) * GetLapseRateMult(object, cov, plan) + GetLapseRateFlatExtra(object, cov, plan)
      w <- ifelse(w < 0, 0, w)
      w <- ifelse(w > 1, 1, w)
      assumpInfo$w.Expd <- w
      return(assumpInfo)
   }
)


setMethod(
   f = "GetPaddAssump",
   signature = "LapseAssump",
   definition = function(object, cov, plan, assumpInfo) {
      pfad <- GetLapsePfad(object, cov, plan)
      w <- assumpInfo$w.Expd * (1 + pfad)
      w <- ifelse(w < 0, 0, w)
      w <- ifelse(w > 1, 1, w)
      assumpInfo$w.Padd <- w
      return(assumpInfo)
   }
)


setMethod(
   f = "GetAssump",
   signature = "LapseAssump",
   definition = function(object, cov, plan, assumpInfo = list()){
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo)
      return(assumpInfo)
   }
)



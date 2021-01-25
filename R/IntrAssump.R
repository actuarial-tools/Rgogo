#' @include IIntrAssump.R
NULL

setClass(
   Class = "IntrAssump",
   contains = "IIntrAssump",
   slots = c(
      IntrRate = "numeric",
      Margin = "numeric"
   )
)

IntrAssump <- function(rate = 0, margin = 0, id = character(0), descrip = character()) {
   assump <- new(
      Class = "IntrAssump",
      IntrRate = rate,
      Margin = margin,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}

setValidity(
   Class = "IntrAssump",
   method = function(object) {
      err <- New.SysMessage()
      # v <- Validator.Length(minLen = 1, maxLen = Global.MaxProjYears())
      v <- Validator.Length(minLen = 1, maxLen = GetValue(Rgogo::Const.MaxProjYears))
      isValid <- Validate(v, object@IntrRate)
      if (!isValid) {
         AddMessage(err) <- "Slot '@IntrRate' in an object of or extending class 'IntrAssump' has invalid length."
      }
      isValid <- Validate(v, object@Margin)
      if (!isValid) {
         AddMessage(err) <- "Slot '@Margin' in an object of or extending class 'IntrAssump' has invalid length."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetIntrRate",
   signature = "IntrAssump",
   definition = function(object) {
      return(object@IntrRate)
   }
)

setMethod(
   f = "GetMargin",
   signature = "IntrAssump",
   definition = function(object) {
      return(object@Margin)
   }
)

setMethod(
   f = "SetIntrRate<-",
   signature = "IntrAssump",
   definition = function(object, value) {
      object@IntrRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetMargin<-",
   signature = "IntrAssump",
   definition = function(object, value) {
      object@Margin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "IntrAssump",
   definition = function(object, assumpInfo, projFreq, projLen) {
      if(length(object@IntrRate) == 1) {
         i <- rep((1 + object@IntrRate) ^ (1 / projFreq) - 1, length.out = projLen)
      } else {
         i <- FillZeroIfNA(rep((1 + object@IntrRate) ^ (1 / projFreq) - 1, each = projFreq), len = projLen)
      }
      assumpInfo$i.Expd <- i
      return(assumpInfo)
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "IntrAssump",
   definition = function(object, assumpInfo, projFreq, projLen) {
      if (length(object@IntrRate) == 1) {
         rate <- rep(object@IntrRate, length.out = projLen)
      } else {
         rate <- RepeatTail(c(object@IntrRate, 0), len = projLen)
      }
      margin <- GetMargin(object)
      if (length(margin) == 1) {
         margin <- rep(margin, length.out = projLen)
      } else {
         margin <- RepeatTail(c(margin, 0), len = projLen)
      }
      i <- rep((1 + (rate - margin)) ^ (1 / projFreq) - 1, each = projFreq, length.out = projLen)
      assumpInfo$i.Padd <- i
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "IntrAssump",
   definition = function(object, assumpInfo = list(), projFreq = 1L, projLen = GetValue(Rgogo::Const.MaxProjYears) * projFreq) {
      stopifnot(projFreq %in% c(1, 2, 4, 12), Is.WholeNumber(projLen))
      assumpInfo <- GetExpdAssump(object, assumpInfo, projFreq, projLen)
      assumpInfo <- GetPaddAssump(object, assumpInfo, projFreq, projLen)
      return(assumpInfo)
   }
)




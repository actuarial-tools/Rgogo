#' @include IIntrAssump.R
NULL


setClass(
   Class = "IntrAssump",
   contains = "IIntrAssump",
   slots = c(
      IntrRate = "numeric",
      IntrPfad = "numeric"
   )
)


IntrAssump <- function(id = character(), rate = 0, pfad = 0) {
   assump <- new(Class = "IntrAssump", Id = id, IntrRate = rate, IntrPfad = pfad)
   return(assump)
}


setValidity(
   Class = "IntrAssump",
   method = function(object) {
      err <- New.SysMessage()
      v <- Validator.Length(minLen = 1, maxLen = Global.MaxProjYears())
      isValid <- Validate(v, object@IntrRate)
      if (!isValid) {
         AddMessage(err) <- "Slot '@IntrRate' in an object of or extending class 'IntrAssump' has invalid length."
      }
      isValid <- Validate(v, object@IntrPfad)
      if (!isValid) {
         AddMessage(err) <- "Slot '@IntrPfad' in an object of or extending class 'IntrAssump' has invalid length."
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
   f = "GetIntrPfad",
   signature = "IntrAssump",
   definition = function(object) {
      return(object@IntrPfad)
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
   f = "SetIntrPfad<-",
   signature = "IntrAssump",
   definition = function(object, value) {
      object@IntrPfad <- value
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
      if (length(object@IntrPfad) == 1) {
         pfad <- rep(object@IntrPfad, length.out = projLen)
      } else {
         pfad <- RepeatTail(c(object@IntrPfad, 0), len = projLen)
      }
      i <- rep((1 + (rate - pfad)) ^ (1 / projFreq) - 1, each = projFreq, length.out = projLen)
      assumpInfo$i.Padd <- i
      return(assumpInfo)
   }
)


setMethod(
   f = "GetAssump",
   signature = "IntrAssump",
   definition = function(object, assumpInfo = list(), projFreq = 1L, projLen = Global.MaxProjYears() * projFreq) {
      stopifnot(projFreq %in% c(1, 2, 4, 12), Is.WholeNumber(projLen))
      assumpInfo <- GetExpdAssump(object, assumpInfo, projFreq, projLen)
      assumpInfo <- GetPaddAssump(object, assumpInfo, projFreq, projLen)
      return(assumpInfo)
   }
)




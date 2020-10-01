#' @include IIntrCredAssump.R
NULL

setClass(
   Class = "IntrCredAssump",
   contains = "IIntrCredAssump",
   slots = c(
      IntrCredRate = "numeric"
   )
)

setValidity(
   Class = "IntrCredAssump",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @IntrCredRate
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 150),
            Validator.Names(hasNames = TRUE)
         ), object@IntrCredRate
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot value @IntrCredRate must be a named numeric vector."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IntrCredAssump <- function(rate = 0, pfad = 0, id = character(0), descrip = character()) {
   assump <- new(
      Class = "IntrCredAssump",
      IntrCredRate = rate,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}

setMethod(
   f = "GetIntrCredRate",
   signature = "IntrCredAssump",
   definition = function(object) {
      return(object@IntrCredRate)
   }
)

setMethod(
   f = "SetIntrCredRate<-",
   signature = "IntrCredAssump",
   definition = function(object, value) {
      if (length(value) == 1 & is.null(names(value))) {
         names(value) <- "1900-01-01"
      }
      object@IntrCredRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "IntrCredAssump",
   definition = function(object, cov, plan, assumpInfo) {
      assumpInfo$i_cred.Expd <- GetIntrCredRate(object, cov, plan)
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "IntrCredAssump",
   definition = function(object, cov, plan) {
      covMonths <- GetCovMonths(plan, cov)
      credRate <- object@IntrCredRate[sort(names(object@IntrCredRate), decreasing = TRUE)]
      rate <- unlist(
         lapply(
            X = as.list(GetIntrCredDate(plan, cov)),
            FUN = function(intrCredDates) {
               rate <- credRate[intrCredDates >= as.Date(names(credRate))][1]
               return(rate)
            }
         )
      )
      return(rate)
   }
)




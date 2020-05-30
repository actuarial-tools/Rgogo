setClassUnion(name = "IModel_or_list", members = c("IModel", "list"))
setClassUnion(name = "IArgSet_or_list", members = c("IArgSet", "list"))


setGeneric(name = "GetArgsList", def = function(object) {standardGeneric("GetArgsList")})
#setGeneric(name = "GetModelList", def = function(object) {standardGeneric("GetModelList")})
setGeneric(name = "GetValuDates", def = function(object) {standardGeneric("GetValuDates")})


setClass(
   Class = "ArgSet.ResProj",
   contains = "IArgSet",
   slots = c(
      ValuArgSet = "IArgSet_or_list",
      ValuModel = "IModel"
   )
)


setValidity(
   Class = "ArgSet.ResProj",
   method = function(object) {
      err <- New.SysMessage()
      # Valuation dates must be in ascending order, and cannot have duplicated values.
      valuDates <- GetValuDates(object)
      isValid <- identical(sort(valuDates), valuDates)
      if (isValid != TRUE) {
         AddMessage(err) <- "@ValuArgSet must contain valuation dates in ascending order."
      }
      isValid <- !any(duplicated(valuDates))
      if (isValid != TRUE) {
         AddMessage(err) <- "@ValuArgSet cannot contain duplicated valuation dates."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


ArgSet.ResProj <- function(valuModel, valuArgSet) {
   args <- new(
      Class = "ArgSet.ResProj",
      ValuModel = valuModel,
      ValuArgSet = valuArgSet
   )
}


setMethod(
   f = "GetArgsList",
   signature = "ArgSet.ResProj",
   definition = function(object) {
      if (is.list(object@ValuArgSet)) {
         return(object@ValuArgSet)
      } else {
         return(list(object@ValuArgSet))
      }
   }
)


setMethod(
   f = "GetModel",
   signature = "ArgSet.ResProj",
   definition = function(object) {
      return(object@ValuModel)
   }
)


setMethod(
   f = "GetValuDates",
   signature = "ArgSet.ResProj",
   definition = function(object) {
      valuDateList <- lapply(GetArgsList(object), function(args) {return(GetArgValue(args, "ValuDate"))})
      return(lubridate::as_date(unlist(valuDateList)))
   }
)

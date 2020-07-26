#' @include IArgSet.R
#' @include IModel.R
NULL


setGeneric(name = "GetTestItem", def = function(object) {standardGeneric("GetTestItem")})
setGeneric(name = "SetTestItem<-", def = function(object, value) {standardGeneric("SetTestItem<-")})


setClass(Class = "Model.ScnrTest", contains = c("IModel"))


Model.ScnrTest <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(
      Class = "Model.ScnrTest",
      Args = args,
      descrip = as.character(descrip)
   )
   SetModelId(model) <- as.character(id)
   return(model)
}


setMethod(
   f = "GetModel",
   signature = "Model.ScnrTest",
   definition = function(object) {
      argModel <- GetArgValue(object, "Model")
      if (is.character(argModel)) {
         modelId <- ifelse(startsWith(argModel, "Model."), argModel, paste0("Model.", argModel))
         return(eval(expr = parse(text = modelId)))
      } else {
         return(argModel)
      }
   }
)


setMethod(
   f = "SetModel<-",
   signature = "Model.ScnrTest",
   definition = function(object, value) {
      object <- SetArgValue(object, Model = value)
      return(object)
   }
)


setMethod(
   f = "GetScnrList",
   signature = "Model.ScnrTest",
   definition = function(object) {
      return(GetArgValue(object, "ScnrList"))
   }
)

setMethod(
   f = "SetScnrList<-",
   signature = "Model.ScnrTest",
   definition = function(object, value) {
      object <- SetArgValue(object, ScnrList = value)
      return(object)
   }
)


setMethod(
   f = "GetTestItem",
   signature = "ArgSet.ScnrTest",
   definition = function(object) {
      return(GetArgValue(object, "TestItem"))
   }
)


setMethod(
   f = "SetTestItem<-",
   signature = "ArgSet.ScnrTest",
   definition = function(object, value) {
      object <- SetArgValue(object, TestItem = value)
      return(object)
   }
)


setMethod(
   f = "Run",
   signature = "Model.ScnrTest",
   definition = function(object, var, result) {
      model <- GetModel(object)
      scnrList <- GetScnrList(object)
      scnrNames <- names(scnrList)
      scnrTestItem <- GetTestItem(object)
      result <- lapply(
         scnrNames,
         function(scnrId, scnList, model, var, testItem) {
            SetArgs(model) <- scnList[[scnrId]]
            rsltModel <- Run(model, var, list())
            rslt <- list()
            for (item in testItem) {
               rslt[[item]] <- cbind(list(ScnrId = scnrId), rsltModel[[item]])
            }
            return(rslt)
         },
         scnrList, model, var, scnrTestItem
      )
      jobResult <- list()
      for (item in scnrTestItem) {
         s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$", item), collapse = ","), ")")
         jobResult[[item]] <- eval(expr = parse(text = s))
      }
      return(jobResult)
   }
)







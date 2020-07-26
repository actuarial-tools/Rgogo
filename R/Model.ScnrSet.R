#' @include IArgSet.R
#' @include IModel.R
NULL
setClass(Class = "Model.ScnrSet", contains = c("IModel"))

Model.ScnrSet <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.ScnrSet", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}


setMethod(
   f = "GetModel",
   signature = "Model.ScnrSet",
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
   signature = "Model.ScnrSet",
   definition = function(object, value) {
      object <- SetArgValue(object, Model = value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetScnrList",
   signature = "Model.ScnrSet",
   definition = function(object) {
      return(GetArgValue(object, "ScnrList"))
   }
)

setMethod(
   f = "SetScnrList<-",
   signature = "Model.ScnrSet",
   definition = function(object, value) {
      object <- SetArgValue(object, ScnrList = value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "Run",
   signature = "Model.ScnrSet",
   definition = function(object, var, result) {
      model <- GetModel(object)
      scnrList <- GetScnrList(object)
      scnrNames <- names(scnrList)
      result <- lapply(
         scnrNames,
         function(scnrId, scnList, model, var) {
            SetArgs(model) <- scnList[[scnrId]]
            rslt <- Run(model, var, list())
            df <- data.frame(
               ScnrId = scnrId,
               ModelId = GetId(model),
               ReportClass1 = GetReportClass1(var),
               CovId = GetId(var),
               Res.Gross = rslt$Res.Gross,
               Res.Rein = rslt$Res.Rein,
               Res.Net = rslt$Res.Net,
               stringsAsFactors = FALSE
            )
            return(df)
         },
         scnrList, model, var
      )
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]"), collapse = ","), ")")
      dfResult <- eval(expr = parse(text = s))
      return(dfResult)
   }
)







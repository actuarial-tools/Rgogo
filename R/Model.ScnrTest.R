setClass(
   Class = "Model.ScnrTest",
   contains = "IModel"
)

Model.ScnrTest <- function(args, id = character(0L), descrip = character(0L)) {
   object <- new(Class = "Model.ScnrTest", Args = args, Descrip = as.character(descrip))
   SetModelId(object) <- as.character(id)
   return(object)
}

setMethod(
   f = "GetModel",
   signature = "Model.ScnrTest",
   definition = function(object) {
      model <- GetArgValue(object@Args, "Model")
      if (is.character(model)) {
         modelId <- ifelse(startsWith(model, "Model."), model, paste0("Model.", model))
         return(eval(expr = parse(text = modelId)))
      } else {
         return(model)
      }
   }
)

setMethod(
   f = "SetModel<-",
   signature = "Model.ScnrTest",
   definition = function(object, value) {
      object <- SetArgValue(object, Model = value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetScnrList",
   signature = "Model.ScnrTest",
   definition = function(object) {
      return(GetArgValue(object@Args, "ScnrList"))
   }
)

setMethod(
   f = "SetScnrList<-",
   signature = "Model.ScnrTest",
   definition = function(object, value) {
      object <- SetArgValue(object, ScnrList = value)
      validObject(object)
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
      result <- lapply(
         scnrNames,
         function(scnrId, scnList, model, var) {
            SetArgs(model) <- scnrList[[scnrId]]
            rslt <- Run(model, var, list())
            if (HasValue(rslt)) {
               eval(
                  expr = parse(
                     text = paste0(
                        "df <- data.frame(ScnrId = scnrId, CovId = GetId(var), ReportClass1 = GetReportClass1(var), ",
                        paste0("`", object@Args@TestItem, "` = rslt$", object@Args@TestItem, collapse = ","),
                        ", stringsAsFactors = FALSE)"
                     )
                  )
               )
               return(df)
            } else {
               return(NULL)
            }
         },
         scnrList, model, var
      )
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]"), collapse = ","), ")")
      dfResult <- eval(expr = parse(text = s))
      return(list(dfResult))
   }
)







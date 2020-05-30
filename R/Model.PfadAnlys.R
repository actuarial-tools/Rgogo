setClass(Class = "Model.PfadAnlys", contains = "IModel")


Model.PfadAnlys <- function(id, args) {
   model <- new(Class = "Model.PfadAnlys", Id = id, Args = args)
   return(model)
}


setMethod(
   f = "Run",
   signature = "Model.PfadAnlys",
   definition = function(object, var, result) {
      # PfAD analysis is to calculate reserves by removing provisions for adverse deviation in the following orders: mortality -> lapse -> interest -> expense
      # Calculate base scenario reserve
      valuModel <- GetModel(GetArgs(object))
      dfResult <- Model.PfadAnlys.NewResultContainer(object, var)
      valuResult <- Run(valuModel, var, list())
      dfResult[1,"Res.Net.Base"] <- valuResult$Res.Net
      valuArgs <- GetArgs(valuModel)
      # Remove mortality pfad
      if (GetArgValue(valuArgs, "ApplyMortMargin") == TRUE) {
         valuModel <- SetArgValue(valuModel, ApplyMortMargin = FALSE)
         valuResult <- Run(valuModel, var, list())
         dfResult[1, "Res.Net.1"] <- valuResult$Res.Net
      } else {
         dfResult[1, "Res.Net.1"] <- dfResult[1, "Base"]
      }
      dfResult[1, "Pfad.Mort"] <- dfResult[1, "Res.Net.Base"] - dfResult[1, "Res.Net.1"]
      # Remove lapse pfad
      if (GetArgValue(valuArgs, "ApplyLapseMargin") == TRUE) {
         valuModel <- SetArgValue(valuModel, ApplyLapseMargin = FALSE)
         valuResult <- Run(valuModel, var, list())
         dfResult[1, "Res.Net.2"] <- valuResult$Res.Net
      } else {
         dfResult[1, "Res.Net.2"] <- dfResult[1, "Base"]
      }
      dfResult[1, "Pfad.Lapse"] <- dfResult[1, "Res.Net.1"] - dfResult[1, "Res.Net.2"]
      # Remove interest pfad
      if (GetArgValue(valuArgs, "ApplyIntrMargin") == TRUE) {
         valuModel <- SetArgValue(valuModel, ApplyIntrMargin = FALSE)
         valuResult <- Run(valuModel, var, list())
         dfResult[1, "Res.Net.3"] <- valuResult$Res.Net
      } else {
         dfResult[1, "Res.Net.3"] <- dfResult[1, "Base"]
      }
      dfResult[1, "Pfad.Intr"] <- dfResult[1, "Res.Net.2"] - dfResult[1, "Res.Net.3"]
      # Remove expense pfad
      if (GetArgValue(valuArgs, "ApplyExpnsMargin") == TRUE) {
         valuModel <- SetArgValue(valuModel, ApplyExpnsMargin = FALSE)
         valuResult <- Run(valuModel, var, list())
         dfResult[1, "Res.Net.4"] <- valuResult$Res.Net
      } else {
         dfResult[1, "Res.Net.4"] <- dfResult[1, "Base"]
      }
      dfResult[1, "Pfad.Expns"] <- dfResult[1, "Res.Net.3"] - dfResult[1, "Res.Net.4"]
      result$PfadInfo <- dfResult
      return(result)
   }
)


Model.PfadAnlys.NewResultContainer <- function(model, cov) {
   df <- data.frame(
      ModelId = GetId(model),
      CovId = GetId(cov),
      ReportClass1 = GetReportClass1(cov),
      Res.Net.Base = NA,
      Res.Net.1 = NA,
      Res.Net.2 = NA,
      Res.Net.3 = NA,
      Res.Net.4 = NA,
      Pfad.Mort = NA,
      Pfad.Lapse = NA,
      Pfad.Intr = NA,
      Pfad.Expns = NA,
      stringsAsFactors = FALSE
   )
   return(df)
}

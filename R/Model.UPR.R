# Unearned Premium Reserve Model
setClass(Class = "Model.UPR", contains = "IModel")


Model.UPR <- function(args = ArgSet.UPR()) {
   model <- new(Class = "Model.UPR", Id = "UPR", Args = args)
   return(model)
}


setMethod(
   f = "Run",
   signature = c("Model.UPR", "Cov"),
   definition = function(object, var, result = list()) {
      # Calculate unearned portion
      projStartDate <- GetArgValue(object, "ValuDate") + 1
      tInfo <- GetProjTimelineInfo(projStartDate, var)
      premDueDates <- GetCovTimeline(tInfo)[((1:length(GetCovProjTimeIndex(tInfo))) - 1) %% (12 / GetPremMode(var)) == 0]
      x <- sum(premDueDates < projStartDate)
      if (x > 0 & x < length(premDueDates)) {
         prevPremDate <- premDueDates[x]
         nextPremDate <- premDueDates[x + 1]
         urndProp <- (lubridate::interval(projStartDate, nextPremDate) / lubridate::days(1)) / (lubridate::interval(prevPremDate, nextPremDate) / lubridate::days(1))
      } else {
         urndProp <- 0
      }
      uprGross <- GetModPrem(var) * urndProp
      uprCeded <- uprGross * ifelse(HasValue(GetReinProp(var)), GetReinProp(var), 0)
      uprNet <- uprGross - uprCeded
      result$Res.Gross <- uprGross
      result$Res.Rein <- uprCeded
      result$Res.Net <- uprNet
      result$UrndProp <- urndProp

      result$CovData <- var
      result$ValuSumm <- .SumrzResult.Model.UPR(object, var, result)

      return(result)
   }
)


.SumrzResult.Model.UPR <- function(model, cov, result) {
   m <- GetPolMonth(GetIssDate(cov), GetArgValue(model, "ValuDate"))
   curCV <- ifelse(is.null(result$Proj$CV), 0, result$Proj$CV[m])
   curDthBen <- ifelse(is.null(result$Proj$Ben.Dth), 0, result$Proj$Ben.Dth[m])
   curReinDthBen <- ifelse(is.null(result$Proj$Rein.Ben), 0, result$Proj$Rein.Ben[m])
   df <- data.frame(
      ModelId = ifelse(length(GetId(model)) > 0, GetId(model), "NA"),
      ArgSetId = ifelse(length(GetId(GetArgs(model))) > 0, GetId(GetArgs(model)), "NA"),
      ValuDate = GetArgValue(model, "ValuDate"),
      ReportClass1 = GetReportClass1(cov),
      CovId = GetId(cov),
      AnlzPrem = ifelse(HasValue(GetPremMode(cov)), GetPremMode(cov), 0) * ifelse(HasValue(GetModPrem(cov)), GetModPrem(cov), 0),
      GrossSumIns = GetFaceAmt(cov),
      # NetSumIns = GetFaceAmt(cov) * (1 - ifelse(HasValue(GetReinProp(cov)), GetReinProp(cov), 0)),
      NetSumIns = GetFaceAmt(cov) * (1 - ifelse(is.null(result$.ReinProp), 0, result$.ReinProp)),
      PUAAmt = GetPUAAmt(cov),
      Res.Gross = result$Res.Gross,
      Res.Rein = result$Res.Rein,
      Res.Net = result$Res.Net,
      AccBal = GetAccBal(cov),
      CV = NA,
      CVDfcn = NA,
      LiabDur = NA,
      Pv.Prem = NA,
      Pv.Prem.Tax = NA,
      Pv.Comm = NA,
      Pv.Comm.Ovrd = NA,
      Pv.Ben.Dth = NA,
      Pv.Ben.Mat = NA,
      Pv.Ben.Sur = NA,
      Pv.Ben.Dth.PUA = NA,
      Pv.Ben.Mat.PUA = NA,
      Pv.Ben.Sur.PUA = NA,
      Pv.Ben.Anu = NA,
      Pv.Expns.Acq = NA,
      Pv.Expns.Mnt = NA,
      Pv.Rein.Ben = NA,
      Pv.Rein.Prem = NA,
      Pv.Rein.Prem.Rfnd = NA,
      Pv.Rein.Comm = NA,
      Pv.Rein.Comm.Rfnd = NA,
      stringsAsFactors = FALSE
   )
   return(df)
}



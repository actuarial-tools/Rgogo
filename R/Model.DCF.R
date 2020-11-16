#' @include Model.CF.R
NULL

setClass(Class = "Model.DCF", contains = "IModel")

Model.DCF <- function(args = ArgSet.DCF(), id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.DCF", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.DCF", "Cov"),
   definition = function(object, var, result = list()) {
      args <- GetArgs(object)
      projStartDate <- GetArgValue(args, "ProjStartDate")
      plan <- GetPlan(var)
      # Run cash flow projection
      modelCF <- Model.CF(args)
      result <- Run(modelCF, var, result = result)

      # Get interest assumption information
      covProjLen <- GetCovProjLen(result$Timeline)
      covProjTimeIndex <- GetCovProjTimeIndex(result$Timeline)
      result <- GetAssump(GetArgValue(args, "IntrAssump"), assumpInfo = result)
      if (GetArgValue(object, "ApplyIntrMargin")) {
         i <- rep(result$i.Padd, each = 12, length.out = covProjLen)
      } else {
         i <- rep(result$i.Expd, each = 12, length.out = covProjLen)
      }
      v <- (1 + i) ^ (-1 / 12)
      t <- covProjTimeIndex[covProjTimeIndex > 0]
      s <- t - as.integer(t)
      v1 <- cumprod(v) * (v ^ ifelse(s == 0, 0, s - 1))    # Discount factors for cashflows at the end of policy month
      if (s[1] == 0) {
         v0 <- ShiftRight(v1, positions = 1, filler = 1)
      } else {
         v0 <- v1
      }
      cf <- result$Cf
      if (!all(cf$Ben.Anu == 0)) {
         if (GetAnuTiming(plan) == 0L) {
            vAnuBen <- v0
         } else {
            vAnuBen <- v1
         }
      } else {
         vAnuBen <- 0
      }
      result$PV <- list(
         CovId = ifelse(length(GetId(var)) > 0, GetId(var), NA),
         Prem = ifelse(is.null(cf$Prem), 0, sum(cf$Prem * v0)),
         Prem.Tax = sum(cf$Prem.Tax * v0),
         Comm =  sum(cf$Comm * v0),
         Comm.Ovrd = sum(cf$Comm.Ovrd * v0),
         Ben.Dth = sum(cf$Ben.Dth * v1),
         Ben.Mat = sum(cf$Ben.Mat * v1),
         Ben.Sur = sum(cf$Ben.Sur * v1),
         Ben.Dth.PUA = sum(cf$Ben.Dth.PUA * v1),
         Ben.Mat.PUA = sum(cf$Ben.Mat.PUA * v1),
         Ben.Sur.PUA = sum(cf$Ben.Sur.PUA * v1),
         Ben.Anu = sum(cf$Ben.Anu * vAnuBen),
         Expns.Acq = sum(cf$Expns.Acq * v0),
         Expns.Mnt = sum(cf$Expns.Mnt * v0),
         Rein.Ben = sum(cf$Rein.Ben * v1),
         Rein.Prem = sum(cf$Rein.Prem * v0),
         Rein.Comm = sum(cf$Rein.Comm * v0),
         Rein.Prem.Rfnd = sum(cf$Rein.Prem.Rfnd * v1),
         Rein.Comm.Rfnd = sum(cf$Rein.Comm.Rfnd * v1)
      )
      result$PV$Total.Gross = result$PV$Prem + result$PV$Prem.Tax + result$PV$Comm + result$PV$Comm.Ovrd +
         result$PV$Ben.Dth + result$PV$Ben.Mat + result$PV$Ben.Sur + result$PV$Ben.Dth.PUA + result$PV$Ben.Mat.PUA +
         result$PV$Ben.Sur.PUA + result$PV$Ben.Anu + result$PV$Expns.Acq + result$PV$Expns.Mnt
      result$PV$Total.Rein = result$PV$Rein.Ben + result$PV$Rein.Prem + result$PV$Rein.Comm + result$PV$Rein.Prem.Rfnd + result$PV$Rein.Comm.Rfnd
      result$PV$Total.Net = result$PV$Total.Gross + result$PV$Total.Rein
      # Save assumption information
      # result$Assump <- cbind(result$Assump, data.frame(i = i, t = t, v0 = v0, v1 = v1, stringsAsFactors = FALSE))
      result$Assump$i <- i
      result$Assump$t <- t
      result$Assump$v0 <- v0
      result$Assump$v1 <- v1

      return(result)
   }
)


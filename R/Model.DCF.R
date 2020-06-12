#' @include Model.CF.R
NULL

setClass(Class = "Model.DCF", contains = "IModel")

Model.DCF <- function(args = ArgSet.DCF()) {
   model <- new(Class = "Model.DCF", Id = "DCF", Args = args)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.DCF", "Cov"),
   definition = function(object, var, result = list()) {
      args <- GetArgs(object)
      projStartDate <- GetArgValue(args, "ProjStartDate")
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
      t <- covProjTimeIndex[covProjTimeIndex >= 0]
      s <- t %% 1
      j <- ShiftRight(i, positions = 1, filler = 0)
      v <- cumprod((1 + j) ^ (-1 / 12)) * (1 + i) ^ (-s)
      cf <- result$Cf
      result$PV <- data.frame(
         CovId = ifelse(length(GetId(var)) > 0, GetId(var), NA),
         Prem = ifelse(is.null(cf$Prem), 0, sum(cf$Prem * v)),
         Prem.Tax = ifelse(is.null(cf$Prem.Tax), 0, sum(cf$Prem.Tax * v)),
         Comm =  ifelse(is.null(cf$Comm), 0, sum(cf$Comm * v)),
         Comm.Ovrd = ifelse(is.null(cf$Comm.Ovrd), 0, sum(cf$Comm.Ovrd * v)),
         Ben.Dth = ifelse(is.null(cf$Ben.Dth), 0, sum(cf$Ben.Dth * v)),
         Ben.Mat = ifelse(is.null(cf$Ben.Mat), 0, sum(cf$Ben.Mat * v)),
         Ben.Sur = ifelse(is.null(cf$Ben.Sur), 0, sum(cf$Ben.Sur * v)),
         Ben.Dth.PUA = ifelse(is.null(cf$Ben.Dth.PUA), 0, sum(cf$Ben.Dth.PUA * v)),
         Ben.Mat.PUA = ifelse(is.null(cf$Ben.Mat.PUA), 0, sum(cf$Ben.Mat.PUA * v)),
         Ben.Sur.PUA = ifelse(is.null(cf$Ben.Sur.PUA), 0, sum(cf$Ben.Sur.PUA * v)),
         Ben.Anu = ifelse(is.null(cf$Ben.Anu), 0, sum(cf$Ben.Anu * v)),
         Expns.Acq = ifelse(is.null(cf$Expns.Acq), 0, sum(cf$Expns.Acq * v)),
         Expns.Mnt = ifelse(is.null(cf$Expns.Mnt), 0, sum(cf$Expns.Mnt * v)),
         Rein.Ben = ifelse(is.null(cf$Rein.Ben), 0, sum(cf$Rein.Ben * v)),
         Rein.Prem = ifelse(is.null(cf$Rein.Prem), 0, sum(cf$Rein.Prem * v)),
         Rein.Comm = ifelse(is.null(cf$Rein.Comm), 0, sum(cf$Rein.Comm * v)),
         Rein.Prem.Rfnd = ifelse(is.null(cf$Rein.Prem.Rfnd), 0, sum(cf$Rein.Prem.Rfnd * v)),
         Rein.Comm.Rfnd = ifelse(is.null(cf$Rein.Comm.Rfnd), 0, sum(cf$Rein.Comm.Rfnd * v)),
         Total.Gross = sum(cf$Total.Gross * v),
         Total.Rein = sum(cf$Total.Rein * v),
         Total.Net = sum(cf$Total.Net * v),
         stringsAsFactors = FALSE
      )
      # Save assumption information
      result$Assump <- cbind(result$Assump, data.frame(i = i, v = v, stringsAsFactors = FALSE))
      return(result)
   }
)


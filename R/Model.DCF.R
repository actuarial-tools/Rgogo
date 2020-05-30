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

      # Calculate present values of cash flows
      result$Pv.Prem <- pvPrem <- ifelse(is.null(result$Cf.Prem), 0, sum(result$Cf.Prem * v))
      result$Pv.Prem.Tax <- pvPremTax <- ifelse(is.null(result$Cf.Prem.Tax), 0, sum(result$Cf.Prem.Tax * v))
      result$Pv.Comm <- pvComm <- ifelse(is.null(result$Cf.Comm), 0, sum(result$Cf.Comm * v))
      result$Pv.Comm.Ovrd <- pvCommOvrd <- ifelse(is.null(result$Cf.Comm.Ovrd), 0, sum(result$Cf.Comm.Ovrd * v))
      result$Pv.Ben.Dth <- pvBenDth <- ifelse(is.null(result$Cf.Ben.Dth), 0, sum(result$Cf.Ben.Dth * v))
      result$Pv.Ben.Mat <- pvBenMat <- ifelse(is.null(result$Cf.Ben.Mat), 0, sum(result$Cf.Ben.Mat * v))
      result$Pv.Ben.Sur <- pvBenSur <- ifelse(is.null(result$Cf.Ben.Sur), 0, sum(result$Cf.Ben.Sur * v))
      result$Pv.Ben.Dth.PUA <- pvBenDthPUA <- ifelse(is.null(result$Cf.Ben.Dth.PUA), 0, sum(result$Cf.Ben.Dth.PUA * v))
      result$Pv.Ben.Mat.PUA <- pvBenMatPUA <- ifelse(is.null(result$Cf.Ben.Mat.PUA), 0, sum(result$Cf.Ben.Mat.PUA * v))
      result$Pv.Ben.Sur.PUA <- pvBenSurPUA <- ifelse(is.null(result$Cf.Ben.Sur.PUA), 0, sum(result$Cf.Ben.Sur.PUA * v))
      result$Pv.Ben.Anu <- pvBenAnu <- ifelse(is.null(result$Cf.Ben.Anu), 0, sum(result$Cf.Ben.Anu * v))
      result$Pv.Expns.Acq <- pvExpnsAcq <- ifelse(is.null(result$Cf.Expns.Acq), 0, sum(result$Cf.Expns.Acq * v))
      result$Pv.Expns.Mnt <- pvExpnsMnt <- ifelse(is.null(result$Cf.Expns.Mnt), 0, sum(result$Cf.Expns.Mnt * v))
      result$Pv.Rein.Ben <- pvReinBen <- ifelse(is.null(result$Cf.Rein.Ben), 0, sum(result$Cf.Rein.Ben * v))
      result$Pv.Rein.Prem <- pvReinPrem <- ifelse(is.null(result$Cf.Rein.Prem), 0, sum(result$Cf.Rein.Prem * v))
      result$Pv.Rein.Comm <- pvReinComm <- ifelse(is.null(result$Cf.Rein.Comm), 0, sum(result$Cf.Rein.Comm * v))
      result$Pv.Rein.Prem.Rfnd <- pvReinPremRfnd <- ifelse(is.null(result$Cf.Rein.Prem.Rfnd), 0, sum(result$Cf.Rein.Prem.Rfnd * v))
      result$Pv.Rein.Comm.Rfnd <- pvReinCommRfnd <- ifelse(is.null(result$Cf.Rein.Comm.Rfnd), 0, sum(result$Cf.Rein.Comm.Rfnd * v))
      result$Pv.Total.Gross <- sum(result$Cf.Total.Gross * v)
      result$Pv.Total.Rein <- sum(result$Cf.Total.Rein * v)
      result$Pv.Total.Net <- sum(result$Cf.Total.Net * v)

      # Save assumption information
      result$Assump <- cbind(result$Assump, data.frame(i = i, v = v, stringsAsFactors = FALSE))
      return(result)
   }
)


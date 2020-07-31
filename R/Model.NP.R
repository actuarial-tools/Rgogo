setClass(Class = "Model.NP", contains = "IModel")

Model.NP <- function(args, id = character(0L), descrip = character((0L))) {
   model <- new(
      Class = "Model.NP",
      Args = args,
      Descrip = as.character(descrip)
   )
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.NP", "Cov"),
   definition = function(object, var, result = list()) {
      args <- GetArgs(object)
      SetProjStartDate(args) <- GetIssDate(var)
      if (HasValue(GetPremMode(args))) {
         SetPremMode(var) <- GetPremMode(args)
      }
      SetModPrem(var) <- 1 / GetPremMode(var)
      model <- Model.CF(args)
      result <- Run(model, var, list())
      covProjLen <- GetCovProjLen(result$Timeline)
      cfDthBen <- result$Cf$Ben.Dth
      if (GetDthTiming(args) == 1L) {
         # 1L: death occurs at the end of policy year; 0L: death is evenly distributed at the end of every policy month
         cfDthBen <- GetYearlyTotal(cfDthBen[2:length(cfDthBen)])
         s <- paste0(paste("0,0,0,0,0,0,0,0,0,0,0", cfDthBen, sep = ","), collapse = ",")
         cfDthBen <- eval(expr = parse(text = paste0("c(0,", s, ")")))
         cfDthBen <- FillZeroIfNA(cfDthBen, len = covProjLen)
         result$Cf$Ben.Dth <- cfDthBen
      }
      result <- GetAssump(GetArgValue(args, "IntrAssump"), assumpInfo = result)
      if (GetArgValue(object, "ApplyIntrMargin")) {
         i <- rep(result$i.Padd, each = 12, length.out = covProjLen)
      } else {
         i <- rep(result$i.Expd, each = 12, length.out = covProjLen)
      }
      j <- ShiftRight(i, positions = 1, filler = 0)
      v <- cumprod((1 + j) ^ (-1 / 12))
      result$Assump <- cbind(result$Assump, data.frame(i = i, v = v, stringsAsFactors = FALSE))
      pvDthBen <- ifelse(is.null(result$Cf$Ben.Dth), 0, sum(result$Cf$Ben.Dth * v))
      pvMatBen <- ifelse(is.null(result$Cf$Ben.Mat), 0, sum(result$Cf$Ben.Mat * v))
      pvAnuBen <- ifelse(is.null(result$Cf$Ben.Anu), 0, sum(result$Cf$Ben.Anu * v))
      pvPrem <- ifelse(is.null(result$Cf$Prem), 0, sum(result$Cf$Prem * v))
      A <- -(pvDthBen + pvMatBen + pvAnuBen)
      a <- pvPrem
      cfPrem <- A / a * result$Cf$Prem
      cfGross <- cfPrem + result$Cf$Ben.Dth + result$Cf$Ben.Mat + result$Cf$Ben.Anu
      cfRein <- rep(0, length.out = length(cfGross))     # Net premium model does not take into account reinsurance
      cfNet <- cfGross
      # Calculate project net premium reserve
      np <- result$Assump$np
      projResGross <- -rev(cumsum(rev(cfGross * v))) / v / np
      projResRein <- -rev(cumsum(rev(cfRein * v))) / v / np
      projResNet <- -rev(cumsum(rev(cfNet * v))) / v / np
      result$Cf <- data.frame(
         Timeline = result$Cf$Timeline,
         Prem = cfPrem,
         Ben.Dth = result$Cf$Ben.Dth,
         Ben.Mat = result$Cf$Ben.Mat,
         Ben.Anu = result$Cf$Ben.Anu,
         Total.Gross = cfGross,
         Total.Rein = cfRein,
         Total.Net = cfNet,
         stringsAsFactors = FALSE
      )
      result$PV <- data.frame(
         CovId = ifelse(length(GetId(var)) > 0, GetId(var), NA),
         Prem = pvPrem,
         Ben.Dth = pvDthBen,
         Ben.Mat = pvMatBen,
         Ben.Anu = pvAnuBen,
         stringsAsFactors = FALSE
      )
      result$NP <- data.frame(
         CovId = ifelse(length(GetId(var)) > 0, GetId(var), NA),
         A = A,
         a = a,
         NP = A / a,
         stringsAsFactors = FALSE
      )
      result$ProjRes <- data.frame(
         Timeline = GetCovProjTimeLabel(result$Timeline),
         ProjRes.Gross = projResGross,
         ProjRes.Rein = projResRein,
         ProjRes.Net = projResNet,
         stringsAsFactors = FALSE
      )
      return(result)
   }
)

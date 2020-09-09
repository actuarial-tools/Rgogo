setClass(Class = "Model.PPM", contains = "IModel")

Model.PPM <- function(args = ArgSet.PPM(), id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.PPM", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.PPM", "Cov"),
   definition = function(object, var, result = list()) {
      args <- GetArgs(object)
      resFloor <- GetArgValue(object, "ResFloor")
      # Run discounted cash flow model to calculate reserves.
      model.dcf <- Model.DCF(args)
      result1 <- Run(model.dcf, var, result)
      result1$Res <- data.frame(
         Res.Gross = -result1$PV$Total.Gross,
         Res.Rein = -result1$PV$Total.Rein,
         Res.Net = max(-result1$PV$Total.Net, resFloor),
         stringsAsFactors = FALSE
      )
      result1$.LapsePfadUsed <- GetLapsePfad(GetArgValue(model.dcf, "LapseAssump"))
      # Re-run by reversing lapse margin if necessary (when applying lapse margin and the coverage has cash values)
      if (GetArgValue(object, "ApplyLapseMargin")) {
         lapseAssump <- GetArgValue(model.dcf, "LapseAssump")
         SetLapsePfad(lapseAssump) <- -GetLapsePfad(lapseAssump)
         model.dcf <- SetArgValue(model.dcf, LapseAssump = lapseAssump)
         result2 <- Run(model.dcf, var, result)
         result2$Res <- data.frame(
            Res.Gross = -result2$PV$Total.Gross,
            Res.Rein = -result2$PV$Total.Rein,
            Res.Net = max(-result2$PV$Total.Net, resFloor),
            stringsAsFactors = FALSE
         )
         result2$.LapsePfadUsed <- GetLapsePfad(GetArgValue(model.dcf, "LapseAssump"))
         if (result2$Res$Res.Net[1] > result1$Res$Res.Net[1]) {
            result <- result2
         } else {
            result <- result1
         }
      } else {
         result <- result1
      }
      # # Reserve projection
      # np <- result$Assump$np
      # v <- result$Assump$v
      # dcfGross <- result$Cf$Total.Gross * v
      # dcfRein <- result$Cf$Total.Rein * v
      # dcfNet <- result$Cf$Total.Net * v
      # projResGross <- -rev(cumsum(rev(dcfGross))) / v / np
      # projResRein <- -rev(cumsum(rev(dcfRein))) / v / np
      # projResNet <- -rev(cumsum(rev(dcfNet))) / v / np
      # projResNet <- ifelse(projResNet <= resFloor, resFloor, projResNet)
      # result$ProjRes <- data.frame(
      #    Timeline = GetCovProjTimeLabel(result$Timeline),
      #    ProjRes.Gross = projResGross,
      #    ProjRes.Rein = projResRein,
      #    ProjRes.Net = projResNet,
      #    stringsAsFactors = FALSE
      # )
      # Summarize results
      result$ValuSumm <- .SumrzResult.Model.PPM(object, var, result)
      return(result)
   }
)

.SumrzResult.Model.PPM <- function(model, cov, result) {
   m <- GetPolMonth(GetIssDate(cov), GetArgValue(model, "ValuDate"))
   curCV <- ifelse(is.null(result$Proj$CV), 0, result$Proj$CV[m])
   curDthBen <- ifelse(is.null(result$Proj$Ben.Dth), 0, result$Proj$Ben.Dth[m])
   curReinDthBen <- ifelse(is.null(result$Proj$Rein.Ben), 0, result$Proj$Rein.Ben[m])
   pv <- result$PV
   res <- result$Res
   df <- data.frame(
      # ModelId = ifelse(length(GetId(model)) > 0, GetId(model), NA),
      # ArgSetId = ifelse(length(GetId(GetArgs(model))) > 0, GetId(GetArgs(model)), "NA"),
      # ValuDate = GetArgValue(model, "ValuDate"),
      ReportClass = ifelse(length(GetReportClass1(cov)) > 0, GetReportClass1(cov), NA),
      CovId = ifelse(length(GetId(cov)) > 0, GetId(cov), NA),
      AnlzPrem = ifelse(HasValue(GetPremMode(cov)), GetPremMode(cov), 0) * ifelse(HasValue(GetModPrem(cov)), GetModPrem(cov), 0),
      GrossSumIns = GetFaceAmt(cov),
      NetSumIns = GetFaceAmt(cov) * (1 - ifelse(is.null(result$.ReinProp), 0, result$.ReinProp)),
      PUAAmt = GetPUAAmt(cov),
      ResGross = res$Res.Gross,
      ResRein = res$Res.Rein,
      ResNet = res$Res.Net,
      AccBal = GetAccBal(cov),
      CV = curCV,
      CVDfcn = curCV - res$Res.Net,
      LiabDur = GetProjLen(result$Timeline),
      PV.Prem = ifelse(is.null(pv$Prem), 0, pv$Prem),
      PV.Prem.Tax = ifelse(is.null(pv$Prem.Tax), 0, pv$Prem.Tax),
      PV.Comm = ifelse(is.null(pv$Comm), 0, pv$Comm),
      PV.Comm.Ovrd = ifelse(is.null(pv$Comm.Ovrd), 0, pv$Comm.Ovrd),
      PV.Ben.Dth = ifelse(is.null(pv$Ben.Dth), 0, pv$Ben.Dth),
      PV.Ben.Dth.PUA = ifelse(is.null(pv$Ben.Dth.PUA), 0, pv$Ben.Dth.PUA),
      PV.Ben.Mat = ifelse(is.null(pv$Ben.Mat), 0, pv$Ben.Mat),
      PV.Ben.Mat.PUA = ifelse(is.null(pv$Ben.Mat.PUA), 0, pv$Ben.Mat.PUA),
      PV.Ben.Sur = ifelse(is.null(pv$Ben.Sur), 0, pv$Ben.Sur),
      PV.Ben.Sur.PUA = ifelse(is.null(pv$Ben.Sur.PUA), 0, pv$Ben.Sur.PUA),
      PV.Ben.Anu = ifelse(is.null(pv$Ben.Anu), 0, pv$Ben.Anu),
      PV.Expns.Acq = ifelse(is.null(pv$Expns.Acq), 0, pv$Expns.Acq),
      PV.Expns.Mnt = ifelse(is.null(pv$Expns.Mnt), 0, pv$Expns.Mnt),
      PV.Rein.Ben = ifelse(is.null(pv$Rein.Ben), 0, pv$Rein.Ben),
      PV.Rein.Prem = ifelse(is.null(pv$Rein.Prem), 0, pv$Rein.Prem),
      PV.Rein.Prem.Rfnd = ifelse(is.null(pv$Rein.Prem.Rfnd), 0, pv$Rein.Prem.Rfnd),
      PV.Rein.Comm = ifelse(is.null(pv$Rein.Comm), 0, pv$Rein.Comm),
      PV.Rein.Comm.Rfnd = ifelse(is.null(pv$Rein.Comm.Rfnd), 0, pv$Rein.Comm.Rfnd),
      stringsAsFactors = FALSE
   )
   return(df)
}

ExportToExcel.Model.PPM <- function(result, dir, annual = TRUE, digits = 0, overwrite = FALSE) {
   dir.create(path = dir, showWarnings = "FALSE", recursive = TRUE)
   filePath <- file.path(dir, paste0(GetId(result$CovData), ".xlsx"))
   if (file.exists(filePath) & overwrite == FALSE) {
      return(FALSE)
   } else {
      wb <- openxlsx::createWorkbook()
      wb <- .ExportPPMResultToExcel.CovData(wb, result)
      wb <- .ExportPPMResultToExcel.ValuSumm(wb, result, digits)
      wb <- ExportToExcel.Proj(result, annual, digits, wb, sheetName = "Proj")
      wb <- ExportToExcel.Cf(result, annual, digits, wb, sheetName = "Cf")
      wb <- .ExportPPMResultToExcel.ProjRes(wb, result, annual, digits)
      wb <- .ExportPPMResultToExcel.Assump(wb, result)
      openxlsx::saveWorkbook(wb, filePath, overwrite = TRUE)
      return(TRUE)
   }
}

.ExportPPMResultToExcel.CovData <- function(wb, result) {
   openxlsx::addWorksheet(wb, "CovData")
   cov <- result$CovData
   r <- 1
   c <- 1
   openxlsx::writeData(wb, sheet = "CovData", x = "Cov Id:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetId(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Plan Id:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetId(GetPlan(cov)), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Plan Name:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetDescrip(GetPlan(cov)), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Issue Date:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetIssDate(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Issue Age:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetIssAge(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Risk Class:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetRiskClass(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Face Amount:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetFaceAmt(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "PUA Amount:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetPUAAmt(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Premium Mode:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetPremMode(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Modal Premium:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetModPrem(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Annualized Premium:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetModPrem(cov) * GetPremMode(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Account Balance:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetAccBal(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Expense Weight:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetExpnsWeight(cov), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = "CovData", x = "Expiry Date:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = "CovData", x = GetExpiryDate(cov), startCol = 2, startRow = r)
   openxlsx::setColWidths(wb, sheet = "CovData", cols = (1:2), widths = c(20, 15))
   return(wb)
}

.ExportPPMResultToExcel.ValuSumm <- function(wb, result, digits) {
   sheetName <- "ValuSumm"
   openxlsx::addWorksheet(wb, sheetName)
   data <- result$ValuSumm
   r <- 1
   c <- 1
   openxlsx::writeData(wb, sheet = sheetName, x = "Model Id:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = data$ModelId, startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = sheetName, x = "ArgSetId:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = data$ArgSetId, startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = sheetName, x = "Valuation Date:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = data$ValuDate, startCol = 2, startRow = r)
   r <- r + 1
   openxlsx::writeData(wb, sheet = sheetName, x = "Reserve Summary", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = "Gross Reserve:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = round(data$Res.Gross, digits), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = sheetName, x = "Ceded Reserve:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = round(data$Res.Rein, digits), startCol = 2, startRow = r)
   openxlsx::writeData(wb, sheet = sheetName, x = "Net Reserve:", startCol = 1, startRow = (r <- r + 1))
   openxlsx::writeData(wb, sheet = sheetName, x = round(data$Res.Net, digits), startCol = 2, startRow = r)
   r <- r + 1
   openxlsx::writeData(wb, sheet = sheetName, x = "Present Value of Cashflow Summary:", startCol = 1, startRow = (r <- r + 1))
   if (!is.null(value <- data$Pv.Prem)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Premium:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Prem.Tax)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Premium Tax:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Comm)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Commission:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Comm.Ovrd)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Override:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Dth)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Death Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Dth.PUA)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "PUA Death Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Mat)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Maturity Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Mat.PUA)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "PUA Maturity Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Sur)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Surrender Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Sur.PUA)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "PUA Surrender Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Ben.Anu)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Annuity Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Expns.Acq)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Acquisition Expense:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Expns.Mnt)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Maintenance Expense:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Rein.Ben)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Reinsurance Benefit:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Rein.Prem)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Reinsurance Premium:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Rein.Prem.Rfnd)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Rein. Premium Refund:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Rein.Comm)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Reinsurance Commission:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   if (!is.null(value <- data$Pv.Rein.Comm.Rfnd)) {
      openxlsx::writeData(wb, sheet = sheetName, x = "Rein. Commission Refund:", startCol = 1, startRow = (r <- r + 1))
      openxlsx::writeData(wb, sheet = sheetName, x = round(value, digits), startCol = 2, startRow = r)
   }
   openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:2), widths = c(30, 10))
   return(wb)
}

.ExportPPMResultToExcel.ProjRes <- function(wb, result, annual, digits) {
   if (annual == TRUE) {
      dfOutput <- data.frame(Timeline = GetYearStartValue(result$ProjRes[, "Timeline"]), stringsAsFactors = FALSE)
      cnames <- names(result$ProjRes)
      for (cname in cnames[cnames != "Timeline"]) {
         dfOutput <- eval(expr = parse(text = paste0("cbind(dfOutput, data.frame(", cname, " = GetYearStartValue(result$ProjRes[, cname]), stringsAsFactors = FALSE))")))
      }
   } else {
      dfOutput <- result$ProjRes
   }
   sheetName <- "ProjRes"
   openxlsx::addWorksheet(wb, sheetName)
   dfOutput <- Round.data.frame(dfOutput, digits)
   openxlsx::writeDataTable(wb, sheet = sheetName, x = dfOutput, startCol = 1, startRow = 1)
   openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(dfOutput)[2]), widths = 12)
   return(wb)
}

.ExportPPMResultToExcel.Assump <- function(wb, result) {
   sheetName <- "Assump"
   openxlsx::addWorksheet(wb, sheetName)
   data <- result$Assump
   openxlsx::writeDataTable(wb, sheet = sheetName, x = data, startCol = 1, startRow = 1)
   openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(data)[2]), widths = 12)
   return(wb)
}






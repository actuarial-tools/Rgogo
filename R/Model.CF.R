setClass(Class = "Model.CF", contains = "IModel")

Model.CF <- function(args = ArgSet.CF()) {
   model <- new(Class = "Model.CF", Args = args)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.CF", "Cov"),
   definition = function(object, var, result) {
      projStartDate <- GetArgValue(object, "ProjStartDate")
      if (projStartDate <= GetExpiryDate(var)) {
         result$CovData <- var
         return(Run.CF(object, GetPlan(var), var, result))
      } else {
         stop("The coverage has expired before projection starting date.")
      }
   }
)

setMethod(
   f = "Run.CF",
   signature = c("Model.CF", "IPlan.LT", "Cov"),
   definition = function(object, plan, cov, result) {
      covMonths <- GetCovMonths(plan, cov)
      projStartDate <- GetArgValue(object, "ProjStartDate")
      result$Timeline <- GetProjTimelineInfo(projStartDate, cov, plan)
      projPolMonths <- (1:(covMonths + 1))[GetCovProjTimeIndex(result$Timeline) >= 0]
      projLen <- GetProjLen(result$Timeline)
      covProjLen <- GetCovProjLen(result$Timeline)

      # Project policy values
      result <- Project(plan, cov, result)
      proj <- result$Proj

      # Get mortality assumption information
      mortAssump <- GetArgValue(object, "MortAssump")
      result <- GetAssump(mortAssump, cov, plan, result)
      if (GetArgValue(object, "ApplyMortMargin")) {
         qRate <- result$q.Padd
      } else {
         qRate <- result$q.Expd
      }
      q <- c(0, Convert_qx(qRate, 12L, "ud"))[1:(covMonths + 1)]

      # Get lapse assumption information
      lapseAssump <- GetArgValue(object, "LapseAssump")
      result <- GetAssump(lapseAssump, cov, plan, result)
      if (GetArgValue(object, "ApplyLapseMargin")) {
         wRate <- result$w.Padd
      } else {
         wRate <- result$w.Expd
      }
      lapseMode <- ifelse(length(GetPremMode(cov)) == 0, 12L, GetPremMode(cov))
      wRate <- Convert_qx(wRate, lapseMode, "ud")
      w <- c(0,unlist(lapply(as.list(wRate), function(x){return(c(rep(0, length.out = 12/lapseMode - 1), x))})))
      w[(covMonths + 1):length(w)] <- 0

      # Get expense assumption information
      expnsAssump <- GetArgValue(object, "ExpnsAssump")
      result <- GetAssump(expnsAssump, cov, plan, result, projStartDate)
      ae <- rep(0, length.out = GetProjLen(result$Timeline))
      me <- rep(0, length.out = GetProjLen(result$Timeline))
      if (GetArgValue(object, "ApplyExpnsMargin")) {
         if (!is.null(result$ae.PerPol.Padd)) {
            ae <- ae + result$ae.PerPol.Padd
         }
         if (!is.null(result$ae.PerFaceAmt.Padd)) {
            ae <- ae + result$ae.PerFaceAmt.Padd
         }
         if (!is.null(result$me.PerPol.Padd)) {
            me <- me + result$me.PerPol.Padd
         }
         if (!is.null(result$me.PerPrem.Padd)) {
            me <- me + result$me.PerPrem.Padd
         }
         if (!is.null(result$me.PerPremAmt.Padd)) {
            me <- me + result$me.PerPremAmt.Padd
         }
      } else {
         if (!is.null(result$ae.PerPol.Expd)) {
            ae <- ae + result$ae.PerPol.Expd
         }
         if (!is.null(result$ae.PerFaceAmt.Expd)) {
            ae <- ae + result$ae.PerFaceAmt.Expd
         }
         if (!is.null(result$me.PerPol.Expd)) {
            me <- me + result$me.PerPol.Expd
         }
         if (!is.null(result$me.PerPrem.Expd)) {
            me <- me + result$me.PerPrem.Expd
         }
         if (!is.null(result$me.PerPremAmt.Expd)) {
            me <- me + result$me.PerPremAmt.Expd
         }
      }

      # Probability of survival and cashflow projection
      p <- 1 - q - w
      np <- ShiftRight(cumprod(p), positions = 1, filler = 1)
      np <- np / np[projPolMonths[1]]
      zeroCf <- rep(0, length.out = projLen - covProjLen)
      if (!is.null(proj$Prem)) {
         cfPrem <- proj$Prem[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfPrem <- c(zeroCf, cfPrem)
         }
      } else {
         cfPrem <- rep(0, length.out = projLen)
      }
      # Premium tax cash flow
      if (!is.null(proj$Prem.Tax)) {
         cfPremTax <- -proj$Prem.Tax[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfPremTax <- c(zeroCf, cfPremTax)
         }
      } else {
         cfPremTax <- rep(0, length.out = projLen)
      }
      # Commission and manager override cash flows
      if (!is.null(proj$Comm)) {
         cfComm <- -proj$Comm[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfComm <- c(zeroCf, cfComm)
         }
      } else {
         cfComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Comm.Ovrd)) {
         cfOvrd <- -proj$Comm.Ovrd[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfOvrd <- c(zeroCf, cfOvrd)
         }
      } else {
         cfOvrd <- rep(0, length.out = projLen)
      }
      # Death benefit cash flow
      if (!is.null(proj$Ben.Dth)) {
         cfDthBen <- -proj$Ben.Dth[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBen <- c(zeroCf, cfDthBen)
         }
      } else {
         cfDthBen <- rep(0, length.out = projLen)
      }
      # PUA Death benefit cash flow
      if (!is.null(proj$Ben.Dth.PUA)) {
         cfDthBenPUA <- -proj$Ben.Dth.PUA[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBenPUA <- c(zeroCf, cfDthBenPUA)
         }
      } else {
         cfDthBenPUA <- rep(0, length.out = projLen)
      }
      # Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat)) {
         cfMatBen <- -proj$Ben.Mat[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBen <- c(zeroCf, cfMatBen)
         }
      } else {
         cfMatBen <- rep(0, length.out = projLen)
      }
      # PUA Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat.PUA)) {
         cfMatBenPUA <- -proj$Ben.Mat.PUA[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBenPUA <- c(zeroCf, cfMatBenPUA)
         }
      } else {
         cfMatBenPUA <- rep(0, length.out = projLen)
      }
      # Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur)) {
         cfSurBen <- -proj$Ben.Sur[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBen <- c(zeroCf, cfSurBen)
         }
      } else {
         cfSurBen <- rep(0, length.out = projLen)
      }
      # PUA Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur.PUA)) {
         cfSurBenPUA <- -proj$Ben.Sur.PUA[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBenPUA <- c(zeroCf, cfSurBenPUA)
         }
      } else {
         cfSurBenPUA <- rep(0, length.out = projLen)
      }
      # Reinsurance cash flows.  Current reinsurance implementation is only for insurance only.  No reinsurance is assumed for annuity.
      if (!is.null(proj$Rein.Ben) & is.null(proj$Ben.Anu)) {
         cfReinBen <- proj$Rein.Ben[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfReinBen <- c(zeroCf, cfReinBen)
         }
      } else {
         cfReinBen <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem) & is.null(proj$Ben.Anu)) {
         cfReinPrem <- -proj$Rein.Prem[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPrem <- c(zeroCf, cfReinPrem)
         }
      } else {
         cfReinPrem <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm) & is.null(proj$Ben.Anu)) {
         cfReinComm <- proj$Rein.Comm[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfReinComm <- c(zeroCf, cfReinComm)
         }
      } else {
         cfReinComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinPremRfnd <- proj$Rein.Prem.Rfnd[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPremRfnd <- c(zeroCf, cfReinPremRfnd)
         }
      } else {
         cfReinPremRfnd <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinCommRfnd <- -proj$Rein.Comm.Rfnd[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinCommRfnd <- c(zeroCf, cfReinCommRfnd)
         }
      } else {
         cfReinCommRfnd <- rep(0, length.out = projLen)
      }
      # Annuity benefit cashflow
      if (!is.null(proj$Ben.Anu)) {
         cfAnuBen <- -proj$Ben.Anu[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         crtnMonths <- GetAnuCrtnMonths(plan)
         if (crtnMonths > 0) {
            crtnPrdEnd <- ifelse(GetAnuTiming(plan) == 0, crtnMonths, crtnMonths + 1)
            if (crtnPrdEnd >= projPolMonths[1]) {
               cfAnuBen[1:(crtnPrdEnd - projPolMonths[1] + 1)] <- -proj$Ben.Anu[projPolMonths[1]:crtnPrdEnd]
            }
         }
         if (projLen > covProjLen) {
            cfAnuBen <- c(zeroCf, cfAnuBen)
         }
      } else {
         cfAnuBen <- rep(0, length.out = projLen)
      }
      result$Proj <- proj[projPolMonths,]

      # Projected expenses and projected expense cashflows
      result %<>% AddProjection(projItem = "Expns.Acq", projValue = ae[(projLen - covProjLen + 1):projLen])
      result %<>% AddProjection(projItem = "Expns.Man", projValue = me[(projLen - covProjLen + 1):projLen])
      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), np[projPolMonths] * p[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), np[projPolMonths] * p[projPolMonths])

      cfTotalGross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfDthBen + cfMatBen + cfSurBen + cfDthBenPUA + cfMatBenPUA + cfSurBenPUA + cfAnuBen + cfAcqExpns + cfMntExpns
      cfTotalRein <- cfReinBen + cfReinPrem + cfReinComm + cfReinPremRfnd + cfReinCommRfnd
      result$Cf <- data.frame(
         Timeline = GetProjTimeLabel(result$Timeline),
         Prem = cfPrem,
         Prem.Tax = cfPremTax,
         Comm = cfComm,
         Comm.Ovrd = cfOvrd,
         Ben.Dth = cfDthBen,
         Ben.Dth.PUA = cfDthBenPUA,
         Ben.Mat = cfMatBen,
         Ben.Mat.PUA = cfMatBenPUA,
         Ben.Sur = cfSurBen,
         Ben.Sur.PUA = cfSurBenPUA,
         Ben.Anu = cfAnuBen,
         Expns.Acq = cfAcqExpns,
         Expns.Mnt = cfMntExpns,
         Total.Gross = cfTotalGross,
         Rein.Prem = cfReinPrem,
         Rein.Comm = cfReinComm,
         Rein.Prem.Rfnd = cfReinPremRfnd,
         Rein.Comm.Rfnd = cfReinCommRfnd,
         Total.Rein = cfTotalRein,
         Total.Net = cfTotalGross + cfTotalRein,
         stringsAsFactors = FALSE
      )

      # Export assumption information
      result$Assump <- data.frame(
         PolMonth = projPolMonths,
         q = q[projPolMonths],
         w = w[projPolMonths],
         p = p[projPolMonths],
         np = np[projPolMonths],
         stringsAsFactors = FALSE
      )
      return(result)
   }
)

ExportToExcel.Cf <- function(result, annual, digits = integer(), wb = NULL, sheetName) {
   df <- result$Cf
   if (annual == TRUE) {
      dfOutput <- data.frame(Timeline = GetYearStartValue(df[, "Timeline"]), stringsAsFactors = FALSE)
   } else {
      dfOutput <- data.frame(Timeline = df[, "Timeline"], stringsAsFactors = FALSE)
   }
   dfOutput <- data.frame(Timeline = ifelse(annual == TRUE, GetYearStartValue(df[, "Timeline"]), df[, "Timeline"]), stringsAsFactors = FALSE)
   cnames <- names(df)
   for (cname in cnames[cnames != "Timeline"]) {
      v <- df[, cname]
      if (any(v != 0)) {
         if (annual == TRUE) {
            v <- GetYearlyTotal(v)
         }
         dfOutput <- eval(expr = parse(text = paste0("cbind(dfOutput, data.frame(", cname, " = v, stringsAsFactors = FALSE))")))
      }
   }
   if (length(digits) > 0) {
      dfOutput <- Round.data.frame(dfOutput, digits)
   }
   if (is.null(wb)) {
      wb <- openxlsx::createWorkbook()     # If wb is null, create an new workbook object
   }
   openxlsx::addWorksheet(wb, sheetName)
   openxlsx::writeDataTable(wb, sheet = sheetName, x = dfOutput, startCol = 1, startRow = 1)
   openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(dfOutput)[2]), widths = 12)
   return(wb)
}

ExportToExcel.Proj <- function(result, annual, digits = integer(), wb = NULL, sheetName) {
   df <- result$Proj
   if (annual == TRUE) {
      dfOutput <- data.frame(Timeline = GetYearStartValue(df[, "Timeline"]), stringsAsFactors = FALSE)
   } else {
      dfOutput <- data.frame(Timeline = df[, "Timeline"], stringsAsFactors = FALSE)
   }
   cnames <- names(df)
   for (cname in cnames[cnames != "Timeline"]) {
      v <- df[, cname]
      if (any(v != 0)) {
         if (annual == TRUE) {
            v <- switch (cname,
                         Prem = GetYearlyTotal(v),
                         Prem.Tax = GetYearlyTotal(v),
                         Comm = GetYearlyTotal(v),
                         Comm.Ovrd = GetYearlyTotal(v),
                         CV = GetYearStartValue(v),
                         Naar = GetYearStartValue(v),
                         Ben.Dth = GetYearStartValue(v),
                         Ben.Sur = GetYearStartValue(v),
                         Ben.Mat = GetYearStartValue(v),
                         Ben.Anu = GetYearlyTotal(v),
                         Rein.Retn = GetYearStartValue(v),
                         Rein.Naar = GetYearStartValue(v),
                         Rein.Prem = GetYearlyTotal(v),
                         Rein.Prem.Rfnd = GetYearlyTotal(v),
                         Rein.Comm = GetYearlyTotal(v),
                         Rein.Comm.Rfnd = GetYearlyTotal(v),
                         Rein.Ben = GetYearStartValue(v),
                         PUA = GetYearStartValue(v),
                         CV.PUA = GetYearStartValue(v),
                         Ben.Dth.PUA = GetYearStartValue(v),
                         Ben.Sur.PUA = GetYearStartValue(v),
                         Ben.Mat.PUA = GetYearStartValue(v),
                         AccBal = GetYearStartValue(v),
                         CredIntr = GetYearlyTotal(v),
                         AdminChrg = GetYearlyTotal(v),
                         Expns.Acq = GetYearlyTotal(v),
                         Expns.Mnt = GetYearlyTotal(v),
                         rep(NA, length.out = length(tLabel))
            )
         }
         dfOutput <- eval(expr = parse(text = paste0("cbind(dfOutput, data.frame(", cname, " = v, stringsAsFactors = FALSE))")))
      }
   }
   if (length(digits) > 0) {
      dfOutput <- Round.data.frame(dfOutput, digits)
   }
   if (is.null(wb)) {
      wb <- openxlsx::createWorkbook()     # If wb is null, create an new workbook object
   }
   openxlsx::addWorksheet(wb, sheetName)
   openxlsx::writeDataTable(wb, sheet = sheetName, x = dfOutput, startCol = 1, startRow = 1)
   openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(dfOutput)[2]), widths = 12)
   return(wb)
}



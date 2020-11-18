setClass(Class = "Model.CF", contains = "IModel")

Model.CF <- function(args = ArgSet.CF(), descrip = character(0L), id = character(0L)) {
   model <- new(Class = "Model.CF", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.CF", "Cov"),
   definition = function(object, var, result) {
      projStartDate <- GetArgValue(object, "ProjStartDate")
      if (projStartDate <= GetExpiryDate(var)) {
         result$CovData <- var
         result$.ArgSet <- GetArgs(object)
         return(Run.CF(object, GetPlan(var), var, result))
      } else {
         stop("The coverage has expired before projection starting date.")
      }
   }
)

setMethod(
   f = "Run.CF",
   signature = c("Model.CF", "IPlan", "Cov"),
   definition = function(object, plan, cov, result) {
      covMonths <- GetCovMonths(plan, cov)
      projStartDate <- GetArgValue(object, "ProjStartDate")
      result$Timeline <- GetProjTimelineInfo(projStartDate, cov, plan)
      projLen <- GetProjLen(result$Timeline)
      covProjLen <- GetCovProjLen(result$Timeline)
      projPolMonths <- GetProjPolMonths(result$Timeline)

      result <- Project(plan, cov, result)
      proj <- result$Proj

      # Get mortality assumption information
      mortAssump <- GetArgValue(object, "MortAssump")
      if (!is.null(mortAssump)) {
         result <- GetAssump(mortAssump, cov, plan, result)
         if (GetArgValue(object, "ApplyMortMargin")) {
            qRate <- result$q.Padd
         } else {
            qRate <- result$q.Expd
         }
         q <- Convert_qx(qRate, 12L, "ud")[1:covMonths]
      } else {
         q <- rep(0, length.out = covMonths)
      }

      # Get lapse assumption information
      lapseAssump <- GetArgValue(object, "LapseAssump")
      if (!is.null(lapseAssump)) {
         result <- GetAssump(lapseAssump, cov, plan, result)
         if (GetArgValue(object, "ApplyLapseMargin")) {
            wRate <- result$w.Padd
         } else {
            wRate <- result$w.Expd
         }
         lapseMode <- ifelse(length(GetPremMode(cov)) == 0, 12L, GetPremMode(cov))
         wRate <- Convert_qx(wRate, lapseMode, "ud")
         w <- unlist(lapply(as.list(wRate), function(x){return(c(rep(0, length.out = 12/lapseMode - 1), x))}))
         w <- w[1:covMonths]
      } else {
         w <- rep(0, length.out = covMonths)
      }

      # Get expense assumption information
      expnsAssump <- GetArgValue(object, "ExpnsAssump")
      ae <- rep(0, length.out = projLen)
      me <- rep(0, length.out = projLen)
      if (!is.null(expnsAssump)) {
         result <- GetAssump(expnsAssump, cov, plan, result, projStartDate)
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
      }

      # Probability of survival and cashflow projection
      p <- 1 - q - w
      pn <- ShiftRight(cumprod(p), positions = 1, filler = 1)
      pn <- pn / pn[projPolMonths[1]]
      zeroCf <- rep(0, length.out = projLen - covProjLen)
      covProjTimeIndex <- GetCovProjTimeIndex(result$Timeline)[1:covMonths]
      IsBegPolMonth <- Is.WholeNumber(covProjTimeIndex[projPolMonths[1]])
      if (!is.null(proj$Prem)) {
         cfPrem <- proj$Prem[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfPrem <- c(zeroCf, cfPrem)
         } else if (!IsBegPolMonth) {
            cfPrem <- ShiftLeft(cfPrem, positions = 1, filler = 0)
         }
      } else {
         cfPrem <- rep(0, length.out = projLen)
      }
      # Premium tax cash flow
      if (!is.null(proj$Prem.Tax)) {
         cfPremTax <- -proj$Prem.Tax[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfPremTax <- c(zeroCf, cfPremTax)
         } else if (!IsBegPolMonth) {
             cfPremTax <- ShiftLeft(cfPremTax, positions = 1, filler = 0)
         }
      } else {
         cfPremTax <- rep(0, length.out = projLen)
      }
      # Commission and manager override cash flows
      if (!is.null(proj$Comm)) {
         cfComm <- -proj$Comm[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfComm <- c(zeroCf, cfComm)
         } else if (!IsBegPolMonth) {
            cfComm <- ShiftLeft(cfComm, positions = 1, filler = 0)
         }
      } else {
         cfComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Comm.Ovrd)) {
         cfOvrd <- -proj$Comm.Ovrd[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfOvrd <- c(zeroCf, cfOvrd)
         } else if (!IsBegPolMonth) {
            cfOvrd <- ShiftLeft(cfOvrd, positions = 1, filler = 0)
         }
      } else {
         cfOvrd <- rep(0, length.out = projLen)
      }
      # Death benefit cash flow
      if (!is.null(proj$Ben.Dth)) {
         cfDthBen <- -proj$Ben.Dth[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBen <- c(zeroCf, cfDthBen)
         }
      } else {
         cfDthBen <- rep(0, length.out = projLen)
      }
      # PUA Death benefit cash flow
      if (!is.null(proj$Ben.Dth.PUA)) {
         cfDthBenPUA <- -proj$Ben.Dth.PUA[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBenPUA <- c(zeroCf, cfDthBenPUA)
         }
      } else {
         cfDthBenPUA <- rep(0, length.out = projLen)
      }
      # Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat)) {
         cfMatBen <- -proj$Ben.Mat[projPolMonths] * pn[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBen <- c(zeroCf, cfMatBen)
         }
      } else {
         cfMatBen <- rep(0, length.out = projLen)
      }
      # PUA Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat.PUA)) {
         cfMatBenPUA <- -proj$Ben.Mat.PUA[projPolMonths] * pn[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBenPUA <- c(zeroCf, cfMatBenPUA)
         }
      } else {
         cfMatBenPUA <- rep(0, length.out = projLen)
      }
      # Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur)) {
         cfSurBen <- -proj$Ben.Sur[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBen <- c(zeroCf, cfSurBen)
         }
      } else {
         cfSurBen <- rep(0, length.out = projLen)
      }
      # PUA Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur.PUA)) {
         cfSurBenPUA <- -proj$Ben.Sur.PUA[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBenPUA <- c(zeroCf, cfSurBenPUA)
         }
      } else {
         cfSurBenPUA <- rep(0, length.out = projLen)
      }
      # Reinsurance cash flows.  Current reinsurance implementation is only for insurance only.  No reinsurance is assumed for annuity.
      if (!is.null(proj$Rein.Ben) & is.null(proj$Ben.Anu)) {
         cfReinBen <- proj$Rein.Ben[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfReinBen <- c(zeroCf, cfReinBen)
         }
      } else {
         cfReinBen <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem) & is.null(proj$Ben.Anu)) {
         cfReinPrem <- -proj$Rein.Prem[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPrem <- c(zeroCf, cfReinPrem)
         } else if (!IsBegPolMonth) {
            cfReinPrem <- ShiftLeft(cfReinPrem, positions = 1, filler = 0)
         }
      } else {
         cfReinPrem <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm) & is.null(proj$Ben.Anu)) {
         cfReinComm <- proj$Rein.Comm[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfReinComm <- c(zeroCf, cfReinComm)
         } else if (!IsBegPolMonth) {
            cfReinComm <- ShiftLeft(cfReinComm, positions = 1, filler = 0)
         }
      } else {
         cfReinComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinPremRfnd <- proj$Rein.Prem.Rfnd[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPremRfnd <- c(zeroCf, cfReinPremRfnd)
         }
      } else {
         cfReinPremRfnd <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinCommRfnd <- -proj$Rein.Comm.Rfnd[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinCommRfnd <- c(zeroCf, cfReinCommRfnd)
         }
      } else {
         cfReinCommRfnd <- rep(0, length.out = projLen)
      }
      # Annuity benefit cashflow
      if (!is.null(proj$Ben.Anu)) {
         crtnMonths <- GetAnuCrtnMonths(plan)
         if (GetAnuTiming(plan) == 0L) {
            cfAnuBen <- -proj$Ben.Anu[projPolMonths] * pn[projPolMonths]    # Annuity benefit payable at the beginning of period
         } else {
            cfAnuBen <- -proj$Ben.Anu[projPolMonths] * pn[projPolMonths] * p[projPolMonths]    # Annuity benefit payable at the end of period
         }
         if (crtnMonths >= projPolMonths[1]) {
            cfAnuBen[1:(crtnMonths - projPolMonths[1] + 1)] <- -proj$Ben.Anu[projPolMonths[1]:crtnMonths]
         }
         if (projLen > covProjLen) {
            cfAnuBen <- c(zeroCf, cfAnuBen)
         } else if (!IsBegPolMonth) {
            cfAnuBen <- ShiftLeft(cfAnuBen, positions = 1, filler = 0)
         }
      } else {
         cfAnuBen <- rep(0, length.out = projLen)
      }
      # Projected expenses and projected expense cashflows
      result$Proj$Expns.Acq = c(rep(NA, covMonths - covProjLen), ae[(projLen - covProjLen + 1):projLen])
      result$Proj$Expns.Mnt = c(rep(NA, covMonths - covProjLen), me[(projLen - covProjLen + 1):projLen])
      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      if (projLen == covProjLen & !IsBegPolMonth) {
         cfAcqExpns <- ShiftLeft(cfAcqExpns, positions = 1, filler = 0)
         cfMntExpns <- ShiftLeft(cfMntExpns, positions = 1, filler = 0)
      }
      cfTotalGross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfDthBen + cfMatBen + cfSurBen + cfDthBenPUA + cfMatBenPUA + cfSurBenPUA + cfAnuBen + cfAcqExpns + cfMntExpns
      cfTotalRein <- cfReinBen + cfReinPrem + cfReinComm + cfReinPremRfnd + cfReinCommRfnd

      result$Cf <- list(
         CovId = rep(ifelse(length(GetId(cov)) > 0, GetId(cov), NA), length.out = projLen),
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
         Rein.Ben = cfReinBen,
         Rein.Prem = cfReinPrem,
         Rein.Comm = cfReinComm,
         Rein.Prem.Rfnd = cfReinPremRfnd,
         Rein.Comm.Rfnd = cfReinCommRfnd,
         Total.Gross = cfTotalGross,
         Total.Rein = cfTotalRein,
         Total.Net = cfTotalGross + cfTotalRein
      )

      result$Assump <- list(
         PolMonth = projPolMonths,
         q = q[projPolMonths],
         w = w[projPolMonths],
         p = p[projPolMonths],
         pn = pn[projPolMonths],
         t = covProjTimeIndex[ceiling(covProjTimeIndex) >= 0]
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



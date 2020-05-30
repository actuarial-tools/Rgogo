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
      if (!is.null(result$Proj.Prem)) {
         cfPrem <- result$Proj.Prem[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfPrem <- c(zeroCf, cfPrem)
         }
         result$Proj.Prem <- result$Proj.Prem[projPolMonths]
      } else {
         cfPrem <- rep(0, length.out = projLen)
      }
      # Premium tax cash flow
      if (!is.null(result$Proj.Prem.Tax)) {
         cfPremTax <- -result$Proj.Prem.Tax[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfPremTax <- c(zeroCf, cfPremTax)
         }
         result$Proj.Prem.Tax <- result$Proj.Prem.Tax[projPolMonths]
      } else {
         cfPremTax <- rep(0, length.out = projLen)
      }
      # Commission and manager override cash flows
      if (!is.null(result$Proj.Comm)) {
         cfComm <- -result$Proj.Comm[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfComm <- c(zeroCf, cfComm)
         }
         result$Proj.Comm <- result$Proj.Comm[projPolMonths]
      } else {
         cfComm <- rep(0, length.out = projLen)
      }
      if (!is.null(result$Proj.Comm.Ovrd)) {
         cfOvrd <- -result$Proj.Comm.Ovrd[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfOvrd <- c(zeroCf, cfOvrd)
         }
         result$Proj.Comm.Ovrd <- result$Proj.Comm.Ovrd[projPolMonths]
      } else {
         cfOvrd <- rep(0, length.out = projLen)
      }
      # Death benefit cash flow
      if (!is.null(result$Proj.Ben.Dth)) {
         cfDthBen <- -result$Proj.Ben.Dth[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBen <- c(zeroCf, cfDthBen)
         }
         result$Proj.Ben.Dth <- result$Proj.Ben.Dth[projPolMonths]
      } else {
         cfDthBen <- rep(0, length.out = projLen)
      }
      # PUA Death benefit cash flow
      if (!is.null(result$Proj.Ben.Dth.PUA)) {
         cfDthBenPUA <- -result$Proj.Ben.Dth.PUA[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBenPUA <- c(zeroCf, cfDthBenPUA)
         }
         result$Proj.PUA <- result$Proj.PUA[projPolMonths]
         result$Proj.Ben.Dth.PUA <- result$Proj.Ben.Dth.PUA[projPolMonths]
      } else {
         cfDthBenPUA <- rep(0, length.out = projLen)
      }
      # Maturity benefit cash flow
      if (!is.null(result$Proj.Ben.Mat)) {
         cfMatBen <- -result$Proj.Ben.Mat[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBen <- c(zeroCf, cfMatBen)
         }
         result$Proj.Ben.Mat <- result$Proj.Ben.Mat[projPolMonths]
      } else {
         cfMatBen <- rep(0, length.out = projLen)
      }
      # PUA Maturity benefit cash flow
      if (!is.null(result$Proj.Ben.Mat.PUA)) {
         cfMatBenPUA <- -result$Proj.Ben.Mat.PUA[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBenPUA <- c(zeroCf, cfMatBenPUA)
         }
         result$Proj.Ben.Mat.PUA <- result$Proj.Ben.Mat.PUA[projPolMonths]
      } else {
         cfMatBenPUA <- rep(0, length.out = projLen)
      }
      # Surrender benefit cash flow
      if (!is.null(result$Proj.Ben.Sur)) {
         cfSurBen <- -result$Proj.Ben.Sur[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBen <- c(zeroCf, cfSurBen)
         }
         result$Proj.Ben.Sur <- result$Proj.Ben.Sur[projPolMonths]
         result$Proj.CV <- result$Proj.CV[projPolMonths]
      } else {
         cfSurBen <- rep(0, length.out = projLen)
      }
      # PUA Surrender benefit cash flow
      if (!is.null(result$Proj.Ben.Sur.PUA)) {
         cfSurBenPUA <- -result$Proj.Ben.Sur.PUA[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBenPUA <- c(zeroCf, cfSurBenPUA)
         }
         result$Proj.Ben.Sur.PUA <- result$Proj.Ben.Sur.PUA[projPolMonths]
         result$Proj.CV.PUA <- result$Proj.CV.PUA[projPolMonths]
      } else {
         cfSurBenPUA <- rep(0, length.out = projLen)
      }
      # Reinsurance cash flows.  Current reinsurance implementation is only for insurance only.  No reinsurance is assumed for annuity.
      if (!is.null(result$Proj.Rein.Ben) & is.null(result$Proj.Ben.Anu)) {
         cfReinBen <- result$Proj.Rein.Ben[projPolMonths] * np[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfReinBen <- c(zeroCf, cfReinBen)
         }
         result$Proj.Rein.Ben <- result$Proj.Rein.Ben[projPolMonths]
      } else {
         cfReinBen <- rep(0, length.out = projLen)
      }
      if (!is.null(result$Proj.Rein.Prem) & is.null(result$Proj.Ben.Anu)) {
         cfReinPrem <- -result$Proj.Rein.Prem[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPrem <- c(zeroCf, cfReinPrem)
         }
         result$Proj.Rein.Prem <- result$Proj.Rein.Prem[projPolMonths]
      } else {
         cfReinPrem <- rep(0, length.out = projLen)
      }
      if (!is.null(result$Proj.Rein.Comm) & is.null(result$Proj.Ben.Anu)) {
         cfReinComm <- result$Proj.Rein.Comm[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfReinComm <- c(zeroCf, cfReinComm)
         }
         result$Proj.Rein.Comm <- result$Proj.Rein.Comm[projPolMonths]
      } else {
         cfReinComm <- rep(0, length.out = projLen)
      }
      if (!is.null(result$Proj.Rein.Prem.Rfnd) & is.null(result$Proj.Ben.Anu)) {
         cfReinPremRfnd <- result$Proj.Rein.Prem.Rfnd[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPremRfnd <- c(zeroCf, cfReinPremRfnd)
         }
         result$Proj.Rein.Prem.Rfnd <- result$Proj.Rein.Prem.Rfnd[projPolMonths]
      } else {
         cfReinPremRfnd <- rep(0, length.out = projLen)
      }
      if (!is.null(result$Proj.Rein.Comm.Rfnd) & is.null(result$Proj.Ben.Anu)) {
         cfReinCommRfnd <- -result$Proj.Rein.Comm.Rfnd[projPolMonths] * np[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinCommRfnd <- c(zeroCf, cfReinCommRfnd)
         }
         result$Proj.Rein.Comm.Rfnd <- result$Proj.Rein.Comm.Rfnd[projPolMonths]
      } else {
         cfReinCommRfnd <- rep(0, length.out = projLen)
      }

      # Annuity benefit cashflow
      if (!is.null(result$Proj.Ben.Anu)) {
         cfAnuBen <- -result$Proj.Ben.Anu[projPolMonths] * np[projPolMonths] * p[projPolMonths]
         crtnMonths <- GetAnuCrtnMonths(plan)
         if (crtnMonths > 0) {
            crtnPrdEnd <- ifelse(GetAnuTiming(plan) == 0, crtnMonths, crtnMonths + 1)
            if (crtnPrdEnd >= projPolMonths[1]) {
               cfAnuBen[1:(crtnPrdEnd - projPolMonths[1] + 1)] <- -result$Proj.Ben.Anu[projPolMonths[1]:crtnPrdEnd]
            }
         }
         if (projLen > covProjLen) {
            cfAnuBen <- c(zeroCf, cfAnuBen)
         }
      } else {
         cfAnuBen <- rep(0, length.out = projLen)
      }

      # Expense cashflows
      result$Proj.Expns.Acq <- ae[(projLen - covProjLen + 1):projLen]
      result$Proj.Expns.Mnt <- me[(projLen - covProjLen + 1):projLen]

      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), np[projPolMonths] * p[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), np[projPolMonths] * p[projPolMonths])


      #### Cashflow results - the following block to be deleted.
      result$Cf.Prem <- cfPrem
      result$Cf.Prem.Tax <- cfPremTax
      result$Cf.Comm <- cfComm
      result$Cf.Comm.Ovrd <- cfOvrd
      result$Cf.Ben.Dth <- cfDthBen
      result$Cf.Ben.Dth.PUA <- cfDthBenPUA
      result$Cf.Ben.Mat <- cfMatBen
      result$Cf.Ben.Mat.PUA <- cfMatBenPUA
      result$Cf.Ben.Sur <- cfSurBen
      result$Cf.Ben.Sur.PUA <- cfSurBenPUA
      result$Cf.Ben.Anu <- cfAnuBen
      result$Cf.Rein.Ben <- cfReinBen
      result$Cf.Rein.Prem <- cfReinPrem
      result$Cf.Rein.Comm <- cfReinComm
      result$Cf.Rein.Prem.Rfnd <- cfReinPremRfnd
      result$Cf.Rein.Comm.Rfnd <- cfReinCommRfnd
      result$Cf.Expns.Acq <- cfAcqExpns
      result$Cf.Expns.Mnt <- cfMntExpns
      result$Cf.Total.Gross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfDthBen + cfMatBen + cfSurBen + cfDthBenPUA + cfMatBenPUA + cfSurBenPUA + cfAnuBen + cfAcqExpns + cfMntExpns
      result$Cf.Total.Rein <- cfReinBen + cfReinPrem + cfReinComm + cfReinPremRfnd + cfReinCommRfnd
      result$Cf.Total.Net <- result$Cf.Total.Gross + result$Cf.Total.Rein
      ####

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


# ExportToExcel.Cf <- function(result, annual, digits = integer(), wb = NULL, sheetName) {
#    dfCf <- result$Cf
#    if (annual == TRUE) {
#       dfOutput <- data.frame(Timeline = GetYearStartValue(dfCf[, "Timeline"]), stringsAsFactors = FALSE)
#       cnames <- names(dfCf)
#       for (cname in cnames[cnames != "Timeline"]) {
#          v <- GetYearlyTotal(dfCf[, cname])
#          if (any(v != 0)) {
#             dfOutput <- eval(expr = parse(text = paste0("cbind(dfOutput, data.frame(", cname, " = v, stringsAsFactors = FALSE))")))
#          }
#       }
#    } else {
#       dfOutput <- dfCf
#    }
#    if (length(digits) > 0) {
#       dfOutput <- Round.data.frame(dfOutput, digits)
#    }
#    if (is.null(wb)) {
#       wb <- openxlsx::createWorkbook()     # If wb is null, create an new workbook object
#    }
#    openxlsx::addWorksheet(wb, sheetName)
#    openxlsx::writeDataTable(wb, sheet = sheetName, x = dfOutput, startCol = 1, startRow = 1)
#    openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(dfOutput)[2]), widths = 12)
#    return(wb)
# }
#
#
# ExportToExcel.Proj <- function(result, annual, digits = integer(), wb = NULL, sheetName) {
#    df <- result$Proj
#    if (annual == TRUE) {
#       dfOutput <- data.frame(Timeline = GetYearStartValue(df[, "Timeline"]), stringsAsFactors = FALSE)
#       cnames <- names(df)
#       for (cname in cnames[cnames != "Timeline"]) {
#          v <- switch (cname,
#                       Prem = GetYearlyTotal(df[,cname]),
#                       Prem.Tax = GetYearlyTotal(df[, cname]),
#                       Comm = GetYearlyTotal(df[, cname]),
#                       Comm.Ovrd = GetYearlyTotal(df[, cname]),
#                       CV = GetYearStartValue(df[, cname]),
#                       Naar = GetYearStartValue(df[, cname]),
#                       Ben.Dth = GetYearStartValue(df[, cname]),
#                       Ben.Sur = GetYearStartValue(df[, cname]),
#                       Ben.Mat = GetYearStartValue(df[, cname]),
#                       Ben.Anu = GetYearlyTotal(df[, cname]),
#                       Rein.Retn = GetYearStartValue(df[, cname]),
#                       Rein.Naar = GetYearStartValue(df[, cname]),
#                       Rein.Prem = GetYearlyTotal(df[,cname]),
#                       Rein.Prem.Rfnd = GetYearlyTotal(df[,cname]),
#                       Rein.Comm = GetYearlyTotal(df[,cname]),
#                       Rein.Comm.Rfnd = GetYearlyTotal(df[,cname]),
#                       Rein.Ben = GetYearStartValue(df[, cname]),
#                       PUA = GetYearStartValue(df[, cname]),
#                       CV.PUA = GetYearStartValue(df[, cname]),
#                       Ben.Dth.PUA = GetYearStartValue(df[, cname]),
#                       Ben.Sur.PUA = GetYearStartValue(df[, cname]),
#                       Ben.Mat.PUA = GetYearStartValue(df[, cname]),
#                       AccBal = GetYearStartValue(df[, cname]),
#                       CredIntr = GetYearlyTotal(df[, cname]),
#                       AdminChrg = GetYearlyTotal(df[, cname]),
#                       Expns.Acq = GetYearlyTotal(df[, cname]),
#                       Expns.Mnt = GetYearlyTotal(df[, cname]),
#                       rep(NA, length.out = length(tLabel))
#          )
#          if (any(v != 0)) {
#             dfOutput <- eval(expr = parse(text = paste0("cbind(dfOutput, data.frame(", cname, " = v, stringsAsFactors = FALSE))")))
#          }
#       }
#    } else {
#       dfOutput <- df
#    }
#    if (length(digits) > 0) {
#       dfOutput <- Round.data.frame(dfOutput, digits)
#    }
#    if (is.null(wb)) {
#       wb <- openxlsx::createWorkbook()     # If wb is null, create an new workbook object
#    }
#    openxlsx::addWorksheet(wb, sheetName)
#    openxlsx::writeDataTable(wb, sheet = sheetName, x = dfOutput, startCol = 1, startRow = 1)
#    openxlsx::setColWidths(wb, sheet = sheetName, cols = (1:dim(dfOutput)[2]), widths = 12)
#    return(wb)
# }
#


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



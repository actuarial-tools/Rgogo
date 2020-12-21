setMethod(
   f = "Run.CF",
   signature = c("Model.CF", "IPlan.IA", "Cov"),
   definition = function(object, plan, cov, result) {
      # Return error if the model contains lapse assumption.
      if (HasValue(GetArgValue(object, "LapseAssump"))) {
         stop("Lapse assumption is not permitted for projecting immediate annuity cash flow.")
      }
      covMonths <- GetCovMonths(plan, cov)
      projStartDate <- GetArgValue(object, "ProjStartDate")
      result$Timeline <- GetProjTimelineInfo(projStartDate, cov, plan)
      projLen <- GetProjLen(result$Timeline)
      covProjLen <- GetCovProjLen(result$Timeline)
      projPolMonths <- GetProjPolMonths(result$Timeline)

      # Project policy values
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
      p <- 1 - q
      pn <- ShiftRight(cumprod(p), positions = 1, filler = 1)
      pn <- pn / pn[projPolMonths[1]]
      zeroCf <- rep(0, length.out = projLen - covProjLen)
      covProjTimeIndex <- GetCovProjTimeIndex(result$Timeline)[1:covMonths]
      IsBegPolMonth <- Is.WholeNumber(covProjTimeIndex[projPolMonths[1]])

      # Premium cash flow
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

      # Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat)) {
         cfMatBen <- -proj$Ben.Mat[projPolMonths] * pn[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBen <- c(zeroCf, cfMatBen)
         }
      } else {
         cfMatBen <- rep(0, length.out = projLen)
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

      # Annuity benefit cashflow
      # if (!is.null(proj$Ben.Anu)) {
      crtnMonths <- GetAnuCrtnMonths(plan)
      if (GetAnuTiming(plan) == 0L) {
         cfAnuBen <- -proj$Ben.Anu[projPolMonths] * pn[projPolMonths]   # Annuity benefit payable at the beginning of period
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
      # } else {
      #    cfAnuBen <- rep(0, length.out = projLen)
      # }

      # if (!is.null(proj$Ben.Anu)) {
      #    crtnMonths <- GetAnuCrtnMonths(plan)
      #    if (GetAnuTiming(plan) == 0L) {
      #       cfAnuBen <- -proj$Ben.Anu[projPolMonths] * pn[projPolMonths]    # Annuity benefit payable at the beginning of period
      #    } else {
      #       cfAnuBen <- -proj$Ben.Anu[projPolMonths] * pn[projPolMonths] * p[projPolMonths]    # Annuity benefit payable at the end of period
      #    }
      #    if (crtnMonths >= projPolMonths[1]) {
      #       cfAnuBen[1:(crtnMonths - projPolMonths[1] + 1)] <- -proj$Ben.Anu[projPolMonths[1]:crtnMonths]
      #    }
      #    if (projLen > covProjLen) {
      #       cfAnuBen <- c(zeroCf, cfAnuBen)
      #    } else if (!IsBegPolMonth) {
      #       cfAnuBen <- ShiftLeft(cfAnuBen, positions = 1, filler = 0)
      #    }
      # } else {
      #    cfAnuBen <- rep(0, length.out = projLen)
      # }

      # Projected expenses and projected expense cashflows
      result$Proj$Expns.Acq = c(rep(NA, covMonths - covProjLen), ae[(projLen - covProjLen + 1):projLen])
      result$Proj$Expns.Mnt = c(rep(NA, covMonths - covProjLen), me[(projLen - covProjLen + 1):projLen])
      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      if (projLen == covProjLen & !IsBegPolMonth) {
         cfAcqExpns <- ShiftLeft(cfAcqExpns, positions = 1, filler = 0)
         cfMntExpns <- ShiftLeft(cfMntExpns, positions = 1, filler = 0)
      }
      cfTotalGross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfDthBen + cfMatBen + cfSurBen + cfAnuBen + cfAcqExpns + cfMntExpns
      cfTotalRein <- rep(0, length.out = projLen)

      result$Cf <- list(
         CovId = rep(ifelse(length(GetId(cov)) > 0, GetId(cov), NA), length.out = projLen),
         Timeline = GetProjTimeLabel(result$Timeline),
         Prem = cfPrem,
         Prem.Tax = cfPremTax,
         Comm = cfComm,
         Comm.Ovrd = cfOvrd,
         Ben.Dth = cfDthBen,
         Ben.Dth.PUA = rep(0, length.out = projLen),
         Ben.Mat = cfMatBen,
         Ben.Mat.PUA = rep(0, length.out = projLen),
         Ben.Sur = cfSurBen,
         Ben.Sur.PUA = rep(0, length.out = projLen),
         Ben.Anu = cfAnuBen,
         Expns.Acq = cfAcqExpns,
         Expns.Mnt = cfMntExpns,
         Rein.Ben = rep(0, length.out = projLen),
         Rein.Prem = rep(0, length.out = projLen),
         Rein.Comm = rep(0, length.out = projLen),
         Rein.Prem.Rfnd = rep(0, length.out = projLen),
         Rein.Comm.Rfnd = rep(0, length.out = projLen),
         Total.Gross = cfTotalGross,
         Total.Rein = cfTotalRein,
         Total.Net = cfTotalGross + cfTotalRein
      )

      result$Assump <- list(
         PolMonth = projPolMonths,
         q = q[projPolMonths],
         w = rep(0, length.out = length(projPolMonths)),
         p = p[projPolMonths],
         pn = pn[projPolMonths],
         t = covProjTimeIndex[ceiling(covProjTimeIndex) >= 0]
      )

      return(result)
   }
)


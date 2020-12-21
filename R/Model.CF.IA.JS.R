setMethod(
   f = "Run.CF",
   signature = c("Model.CF", "IPlan.IA.JS", "Cov"),
   definition = function(object, plan, cov, result) {
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
            qRate_x <- result$q_x.Padd
            qRate_y <- result$q_y.Padd
         } else {
            qRate_x <- result$q_x.Expd
            qRate_y <- result$q_y.Expd
         }
         q_x <- Convert_qx(qRate_x, 12L, "ud")[1:covMonths]
         q_y <- Convert_qx(qRate_y, 12L, "ud")[1:covMonths]
      } else {
         q_y <- q_x <- rep(0, length.out = covMonths)
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

      # Probability of survival and cash flow projection
      prob <- GetLifeProb.2L(q_x, q_y)

      p.x <- prob$p_x
      pn.x <- ShiftRight(prob$t_p_x, positions = 1, filler = 1)
      pn.x <- pn.x / pn.x[projPolMonths[1]]

      p.y <- prob$p_y
      pn.y <- ShiftRight(prob$t_p_y, positions = 1, filler = 1)
      pn.y <- pn.y / pn.y[projPolMonths[1]]

      p.xy.ls <- prob$ls.p_xy
      pn.xy.ls <- ShiftRight(prob$ls.t_p_xy, positions = 1, filler = 1)
      pn.xy.ls <- pn.xy.ls / pn.xy.ls[projPolMonths[1]]

      p.xy.jl <- prob$jl.p_xy
      pn.xy.jl <- ShiftRight(prob$jl.t_p_xy, positions = 1, filler = 1)
      pn.xy.jl <- pn.xy.jl / pn.xy.jl[projPolMonths[1]]

      zeroCf <- rep(0, length.out = projLen - covProjLen)
      covProjTimeIndex <- GetCovProjTimeIndex(result$Timeline)[1:covMonths]
      IsBegPolMonth <- Is.WholeNumber(covProjTimeIndex[projPolMonths[1]])

      # Premium cash flow
      if (!is.null(proj$Prem)) {
         cfPrem <- proj$Prem[projPolMonths] * pn.xy.ls[projPolMonths]
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
         cfPremTax <- -proj$Prem.Tax[projPolMonths] * pn.xy.ls[projPolMonths]
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
         cfComm <- -proj$Comm[projPolMonths] * pn.xy.ls[projPolMonths]
         if (projLen > covProjLen) {
            cfComm <- c(zeroCf, cfComm)
         } else if (!IsBegPolMonth) {
            cfComm <- ShiftLeft(cfComm, positions = 1, filler = 0)
         }
      } else {
         cfComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Comm.Ovrd)) {
         cfOvrd <- -proj$Comm.Ovrd[projPolMonths] * pn.xy.ls[projPolMonths]
         if (projLen > covProjLen) {
            cfOvrd <- c(zeroCf, cfOvrd)
         } else if (!IsBegPolMonth) {
            cfOvrd <- ShiftLeft(cfOvrd, positions = 1, filler = 0)
         }
      } else {
         cfOvrd <- rep(0, length.out = projLen)
      }

      # Annuity benefit cash flow
      anu.x <- proj$Ben.Anu[projPolMonths] * pn.x[projPolMonths]
      anu.y <- proj$Ben.Anu.Survr[projPolMonths] * pn.y[projPolMonths]
      anu.xy.jl <- proj$Ben.Anu.Survr[projPolMonths] * pn.xy.jl[projPolMonths]
      if (GetAnuTiming(plan) == 1L) {
         anu.x <- anu.x * p.x[projPolMonths]
         anu.y <- anu.y * p.y[projPolMonths]
         anu.xy.jl <- anu.xy.jl * p.xy.jl[projPolMonths]
      }
      if (GetLifeStatus(cov) == 1L & GetLifeStatus2(cov) == 1L) {
         # If both lives are alive
         cfAnuBen <- -(anu.x + (anu.y - anu.xy.jl))
      } else if (GetLifeStatus(cov) == 1L & GetLifeStatus2(cov) == 0L) {
         # If only life one is alive
         cfAnuBen <- -anu.x
      } else if (GetLifeStatus(cov) == 0L & GetLifeStatus2(cov) == 1L) {
         # If only life two is alive
         cfAnuBen <- -anu.y
      }
      if (projLen > covProjLen) {
         cfAnuBen <- c(zeroCf, cfAnuBen)
      } else if (!IsBegPolMonth) {
         cfAnuBen <- ShiftLeft(cfAnuBen, positions = 1, filler = 0)
      }

      # Projected expenses and projected expense cash flows
      result$Proj$Expns.Acq = c(rep(NA, covMonths - covProjLen), ae[(projLen - covProjLen + 1):projLen])
      result$Proj$Expns.Mnt = c(rep(NA, covMonths - covProjLen), me[(projLen - covProjLen + 1):projLen])
      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), pn.xy.ls[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), pn.xy.ls[projPolMonths])
      if (projLen == covProjLen & !IsBegPolMonth) {
         cfAcqExpns <- ShiftLeft(cfAcqExpns, positions = 1, filler = 0)
         cfMntExpns <- ShiftLeft(cfMntExpns, positions = 1, filler = 0)
      }
      cfTotalGross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfAnuBen + cfAcqExpns + cfMntExpns
      cfTotalRein <- rep(0, length.out = projLen)

      result$Cf <- list(
         CovId = rep(ifelse(length(GetId(cov)) > 0, GetId(cov), NA), length.out = projLen),
         Timeline = GetProjTimeLabel(result$Timeline),
         Prem = cfPrem,
         Prem.Tax = cfPremTax,
         Comm = cfComm,
         Comm.Ovrd = cfOvrd,
         Ben.Dth = rep(0, length.out = projLen),
         Ben.Dth.PUA = rep(0, length.out = projLen),
         Ben.Mat = rep(0, length.out = projLen),
         Ben.Mat.PUA = rep(0, length.out = projLen),
         Ben.Sur = rep(0, length.out = projLen),
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
         q_x = prob$q_x[projPolMonths],
         p_x = prob$p_x[projPolMonths],
         t_p_x = prob$t_p_x[projPolMonths],
         q_y = prob$q_y[projPolMonths],
         p_y = prob$p_y[projPolMonths],
         t_p_y = prob$t_p_y[projPolMonths],
         jl.t_p_xy = prob$jl.t_p_xy[projPolMonths],
         jl.p_xy = prob$jl.p_xy[projPolMonths],
         jl.q_xy = prob$jl.q_xy[projPolMonths],
         ls.t_p_xy = prob$ls.t_p_xy[projPolMonths],
         ls.p_xy = prob$ls.p_xy[projPolMonths],
         ls.q_xy = prob$ls.q_xy[projPolMonths],
         t = covProjTimeIndex[ceiling(covProjTimeIndex) >= 0]
      )

      return(result)
   }
)



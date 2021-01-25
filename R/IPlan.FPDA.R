setClass(
   Class = "IPlan.FPDA",
   contains = "IPlan.Anu",
   slots = c(
      Fund = "IPlan.Fund"
   )
)

IPlan.FPDA <- function(premYears = NA, premToAge = NA, premLoadSchd = numeric(0L),
                       expnsChrgSchd = numeric(0L), expnsChrgMode = 1L, expnsChrgTiming = 0L, expnsChrgType = 0L,
                       surChrgSchd = numeric(0L), minIntrCredRate = numeric(0L),
                       anuYears = NA, anuToAge = NA, anuPremTable = character(0L),
                       anuMode = 12L, anuTiming = 0L, crtnMonths = 0L, anuBenSchd = numeric(0L),
                       premTaxRate = numeric(0L),
                       commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                       id = character(0L), descrip = character(0L)) {
   # Define premium period
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   stopifnot(length(premPeriod) > 0)
   # Define beginning of annuity payout period, starting immediately after premium period.
   anuStart <- c(AnuStartYear = premYears + 1, AnuStartAge = as.integer(premToAge))
   anuStart <- anuStart[!is.na(anuStart)]
   stopifnot(length(anuStart) == 1)
   # Define end of annuity payout period, which is also end of coverage period
   covPeriod <- c(CovYears = anuYears, CovToAge = anuToAge)
   covPeriod <- covPeriod[!is.na(covPeriod)]
   stopifnot(length(covPeriod) > 0)
   # Create the object
   plan <- new(
      Class = "IPlan.FPDA",
      CovPeriod = covPeriod,
      PremPeriod = premPeriod,
      AnuStart = anuStart,
      AnuMode = anuMode,
      AnuTiming = anuTiming,
      CrtnMonths = crtnMonths,
      AnuBenSchd = anuBenSchd,
      PremTable = anuPremTable,
      Fund = IPlan.Fund(
         covYears = premYears,
         covToAge = premToAge,
         premLoadSchd = premLoadSchd,
         expnsChrgSchd = expnsChrgSchd,
         expnsChrgMode = expnsChrgMode,
         expnsChrgTiming = expnsChrgTiming,
         expnsChrgType = expnsChrgType,
         surChrgSchd = surChrgSchd,
         minIntrCredRate = minIntrCredRate,
         commSchd = commSchd,
         ovrdOnPremSchd = ovrdOnPremSchd,
         ovrdOnCommSchd = ovrdOnCommSchd,
         premTaxRate = premTaxRate
      )
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "ProjFund",
   signature = "IPlan.FPDA",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      fundProj <- Project(object@Fund, cov, list(.ArgSet = resultContainer$.ArgSet))
      resultContainer$Proj <- c(
         resultContainer$Proj,
         lapply(
            fundProj$Proj[names(fundProj$Proj) != "Timeline"],
            function(v, len) {
               return(FillTail(v, 0, len))
            },
            covMonths
         )
      )
      return(resultContainer)
   }
)

setMethod(
   f = "ProjAnuBen",
   signature = "IPlan.FPDA",
   definition = function(object, cov, resultContainer) {
      anuPremTable <- GetPremTable(object, cov)
      if (!is.null(anuPremTable)) {
         fundBal <- resultContainer$Proj$Ben.Mat[GetCovMonths(object@Fund, cov)]
         anuRate <- LookUp(tbl = anuPremTable, lookUpKey = list(IssAge = GetAnutzAge(object, cov)))
         SetFaceAmt(cov) <- anuRate[1] * fundBal * 12
         resultContainer <- ProjAnuBen(as(object, "IPlan.Anu"), cov, resultContainer)
         resultContainer$Proj$Ben.Mat <- NULL
      }
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.FPDA",
   definition = function(object, cov, resultContainer = list()) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjFund(object, cov, resultContainer)
      resultContainer <- ProjAnuBen(object, cov, resultContainer)
      return(resultContainer)
   }
)


setClass(
   Class = "IPlan.RT",
   contains = "IPlan.LT",
   slots = c(
      RenewalPeriod = "integer"
   )
)

setValidity(
   Class = "IPlan.RT",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Range(minValue = 1L), object@RenewalPeriod)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid renewal period.  Must be a positive integer."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.RT <- function(renewalPeriod, covToAge,
                     premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                     polFee = numeric(0), premTaxRate = numeric(0L),
                     commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                     rein = character(0L), id = character(0L), descrip = character(0L)) {
   plan <- new(
      Class = "IPlan.RT",
      CovPeriod = c(CovToAge = covToAge),
      PremPeriod = c(PremToAge = covToAge),
      RenewalPeriod = as.integer(renewalPeriod),
      PremTable = premTable,
      ModFactor = modFactor,
      PolFee = polFee,
      PremTaxRate = premTaxRate,
      CommSchd = commSchd,
      OvrdOnPremSchd = ovrdOnPremSchd,
      OvrdOnCommSchd = ovrdOnCommSchd,
      Rein = rein,
      Descrip = as.character(descrip)
   )
   return (plan)
}

setMethod(
   f = "GetPremRate",
   signature = "IPlan.RT",
   definition = function(object, cov) {
      premTable <- GetPremTable(object, cov)
      if (!is(premTable, "Table.IA") & !is(premTable, "Table.AA")) {
         stop("IPlan.RT GetPremRate method currently only handles premium rate table that is an issue age table or an attained age table.")
      }
      tableAges <- as.integer(dimnames(premTable@TValue)[[1]])
      renewalAges <- tableAges[(tableAges >= cov@IssAge) & ((tableAges - cov@IssAge) %% object@RenewalPeriod == 0)]
      premRate <- premTable@TValue[as.character(rep(renewalAges, each = object@RenewalPeriod * 12)[1:GetPremMonths(object, cov)]), 1] / premTable@TBase
      return(premRate)
   }
)

setMethod(
   f = "ProjPrem",
   signature = "IPlan.RT",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      premMonths <- GetPremMonths(object, cov)
      if (is.null(resultContainer$.ArgSet)) {
         curPolMonth <- 1
      } else {
         if (is.null(projStartDate <- GetArgValue(resultContainer$.ArgSet, "ProjStartDate"))) {
            curPolMonth <- 1
         } else {
            curPolMonth <- GetPolMonth(GetIssDate(cov), projStartDate - lubridate::days(1))
         }
      }
      premRate <- GetPremRate(object, cov)
      premMode <- GetPremMode(cov)
      # modPolFee <- GetPolFee(object, premMode)
      # modPremCalcFromTable <- GetFaceAmt(cov) * premRate * GetModFactor(object, premMode)
      modPremCalcFromTable <- GetFaceAmt(cov) * premRate * GetModFactor(object, premMode) + GetPolFee(object, premMode)
      polMonths <- 1:covMonths
      isPremDue <- (polMonths - 1) %% (12 / premMode) == 0 & (polMonths <= premMonths)
      modPrem <- GetModPrem(cov)
      if (HasValue(modPrem)) {
         # projModPrem <- FillZeroIfNA((modPrem - modPolFee) / modPremCalcFromTable[curPolMonth] * modPremCalcFromTable + modPolFee, len = covMonths) * isPremDue
         projModPrem <- FillZeroIfNA(modPrem / modPremCalcFromTable[curPolMonth] * modPremCalcFromTable, len = covMonths) * isPremDue
      } else {
         # projModPrem <- FillZeroIfNA(modPremCalcFromTable + modPolFee, len = covMonths) * isPremDue
         projModPrem <- FillZeroIfNA(modPremCalcFromTable, len = covMonths) * isPremDue
      }
      resultContainer$Proj$Prem <- projModPrem
      resultContainer$Proj$Prem.Tax <- projModPrem * GetPremTaxRate(object, cov)
      return(resultContainer)
   }
)





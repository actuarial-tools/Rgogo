#' @include IPlan.End.Par.R
NULL

setClass(Class = "IPlan.WL.Par", contains = "IPlan.End.Par")

IPlan.WL.Par <- function(planId, ultAge = 100, premYears = NA, premToAge = NA) {
   if (is.na(premYears) & is.na(premToAge)) {
      premToAge <- ultAge
   }
   covPeriod <- c(CovToAge = as.integer(ultAge))
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(
      Class = "IPlan.WL.Par",
      CovPeriod = covPeriod,
      PremPeriod = premPeriod,
      PremTable = premTable,
      ModFactor = modFactor,
      PolFee = polFee,
      PremTaxRate = premTaxRate,
      CVTable = cvTable,
      PUA = pua,
      CommSchd = commSchd,
      OvrdOnPremSchd = ovrdOnPremSchd,
      OvrdOnCommSchd = ovrdOnCommSchd,
      Rein = rein,
      Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(planId)
   return(plan)
}



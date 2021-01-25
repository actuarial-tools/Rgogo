#' @include IPlan.End.Par.R
NULL

setClass(Class = "IPlan.WL.Par", contains = "IPlan.End.Par")

IPlan.WL.Par <- function(ultAge = 100L, premYears = NA, premToAge = NA,
                         premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                         polFee = numeric(0), premTaxRate = numeric(0L), cvTable = character(0L), surChrgSchd = numeric(0L), pua = character(0L),
                         commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                         rein = character(0L), id = character(0L), descrip = character(0L)) {
   if (is.na(premYears) & is.na(premToAge)) {
      premToAge <- ultAge
   }
   covPeriod <- c(CovToAge = as.integer(ultAge))
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(
      Class = "IPlan.WL",
      CovPeriod = covPeriod,
      PremPeriod = premPeriod,
      PremTable = premTable,
      ModFactor = modFactor,
      PolFee = polFee,
      CVTable = cvTable,
      SurChrgSchd = surChrgSchd,
      CommSchd = commSchd,
      OvrdOnPremSchd = ovrdOnPremSchd,
      OvrdOnCommSchd = ovrdOnCommSchd,
      PremTaxRate = premTaxRate,
      Rein = rein,
      Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}


#' @include IPlan.End.Par.R
NULL


setClass(
   Class = "IPlan.WL.Par", contains = "IPlan.End.Par")


IPlan.WL.Par <- function(planId, ultAge = 100, premYears = NA, premToAge = NA) {
   if (is.na(premYears) & is.na(premToAge)) {
      premToAge <- ultAge
   }
   covPeriod <- c(CovToAge = as.integer(ultAge))
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(
      Class = "IPlan.WL.Par",
      Id = as.character(planId),
      CovPeriod = covPeriod,
      PremPeriod = premPeriod
   )
   return(plan)
}



#' @include IPlan.End.R
NULL


setClass(
   Class = "IPlan.WL", contains = "IPlan.End")


IPlan.WL <- function(planId, ultAge = 100L, premYears = NA, premToAge = NA) {
   if (is.na(premYears) & is.na(premToAge)) {
      premToAge <- ultAge
   }
   covPeriod <- c(CovToAge = as.integer(ultAge))
   premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(
      Class = "IPlan.WL",
      Id = as.character(planId),
      CovPeriod = covPeriod,
      PremPeriod = premPeriod
   )
   return(plan)
}



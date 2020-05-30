setClass(
   Class = "IPlan",
   contains = c("IObject", "VIRTUAL")
)


setMethod(
   f = "GetPlanId",
   signature = "IPlan",
   definition = function(object) {
      return(GetId(object))
   }
)


setMethod(
   f = "SetPlanId<-",
   signature = c("IPlan", "character"),
   definition = function(object, value) {
      SetId(object) <- value
      return(object)
   }
)


setMethod(
   f = "GetRiskClass",
   signature = "IPlan",
   definition = function(object, cov) {
      stop("Method 'GetRiskClass' must be implemented by a child class of 'IPlan'.")
   }
)


setMethod(
   f = "GetCovYears",
   signature = "IPlan",
   definition = function(object, cov, rounding = "no") {
      stop("Method 'GetCovYears' must be implemented by a child class of 'IPlan'.")
   }
)


setMethod(
   f = "GetPremYears",
   signature = "IPlan",
   definition = function(object, cov, rounding = "no") {
      stop("Method 'GetPremYears' must be implemented by a child class of 'IPlan'.")
   }
)


setMethod(
   f = "GetCovMonths",
   signature = "IPlan",
   definition = function(object, cov) {
      return(round(GetCovYears(object, cov) * 12, digits = 0))
   }
)


setMethod(
   f = "GetPremMonths",
   signature = "IPlan",
   definition = function(object, cov) {
      return(round(GetPremYears(object, cov) * 12, digits = 0))
   }
)


setMethod(
   f = "GetExpiryDate",
   signature = "IPlan",
   definition = function(object, cov) {
      return(GetIssDate(cov) %m+% months(as.integer(GetCovMonths(object, cov))))
   }
)


setMethod(
   f = "Project",
   signature = "IPlan",
   definition = function(object, ...) {
      stop("Method 'Project' must be implemented by a child class of 'IPlan'.")
   }
)


setMethod(
   f = "SaveAsRda",
   signature = "IPlan",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(planID <- GetPlanId(object)))
      rdaName <- paste0("Plan.", planID)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)

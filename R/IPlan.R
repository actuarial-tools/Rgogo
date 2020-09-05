setClass(
   Class = "IPlan",
   contains = c("IObject", "VIRTUAL")
)

setValidity(
   Class = "IPlan",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "Plan.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'Plan.'"
         }
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
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
      if (length(value) == 0) return(object)
      if (!startsWith(value, "Plan.")) {
         value <- paste0("Plan.", value)
      }
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


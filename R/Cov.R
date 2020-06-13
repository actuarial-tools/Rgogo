#' @include IPlan.R
#' @include IObject.R
NULL

setClass(
   Class = "Cov",
   contains = "IObject",
   slots = c(PlanId = "character",
             IssDate = "Date",
             IssAge = "integer",
             RiskClass = "character",
             FaceAmt = "numeric",
             ModPrem = "numeric",
             PremMode = "integer",
             ReinProp = "numeric",
             ExpnsWeight = "numeric",
             PUAAmt = "numeric",
             AccBal = "numeric",
             ReportClass1 = "character"
   )
)

Cov <- function(...) {
   cov <- new(Class = "Cov")
   args <- list(...)
   argNames <- names(args)
   for (argName in argNames[argNames %in% slotNames(cov)]) {
      slot(cov, argName) <- args[[argName]]
   }
   validObject(cov)
   return(cov)
}

setMethod(
   f = "GetCovId",
   signature = "Cov",
   definition = function(object) {
      return(object@Id)
   }
)

setMethod(
   f = "SetCovId<-",
   signature = "Cov",
   definition = function(object, value) {
      object@Id <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPlan",
   signature = "Cov",
   definition = function(object) {
      planId <- ifelse(startsWith(object@PlanId, "Plan."), object@PlanId, paste0("Plan.", object@PlanId))
      return(eval(expr = parse(text = planId)))
   }
)

setMethod(
   f = "SetPlan<-",
   signature = c("Cov", "IPlan"),
   definition = function(object, value) {
      object@PlanId <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPlan<-",
   signature = c("Cov", "character"),
   definition = function(object, value) {
      if (startsWith(value, "Plan.")) {
         value <- substr(value, 6, nchar(value))
      }
      object@PlanId <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIssDate",
   signature = "Cov",
   definition = function(object) {
      return(object@IssDate)
   }
)

setMethod(
   f = "SetIssDate<-",
   signature = "Cov",
   definition = function(object, value) {
      object@IssDate <- as.Date(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIssAge",
   signature = "Cov",
   definition = function(object) {
      return(object@IssAge)
   }
)

setMethod(
   f = "SetIssAge<-",
   signature = "Cov",
   definition = function(object, value) {
      object@IssAge <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "Cov",
   definition = function(object) {
      return(object@RiskClass)
   }
)

setMethod(
   f = "SetRiskClass<-",
   signature = "Cov",
   definition = function(object, value) {
      object@RiskClass <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetFaceAmt",
   signature = "Cov",
   definition = function(object) {
      return(object@FaceAmt)
   }
)

setMethod(
   f = "SetFaceAmt<-",
   signature = "Cov",
   definition = function(object, value) {
      object@FaceAmt <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremMode",
   signature = "Cov",
   definition = function(object) {
      return(object@PremMode)
   }
)

setMethod(
   f = "SetPremMode<-",
   signature = "Cov",
   definition = function(object, value) {
      object@PremMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetModPrem",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@ModPrem)) {
         return(object@ModPrem)
      } else {
         return(GetModPrem(GetPlan(object), object))
      }
   }
)

setMethod(
   f = "SetModPrem<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ModPrem <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReinProp",
   signature = "Cov",
   definition = function(object) {
      return(object@ReinProp)
   }
)

setMethod(
   f = "SetReinProp<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReinProp <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsWeight",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@ExpnsWeight)) {
         return(object@ExpnsWeight)
      } else {
         return(1)
      }
   }
)

setMethod(
   f = "SetExpnsWeight<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ExpnsWeight <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPUAAmt",
   signature = "Cov",
   definition = function(object) {
      return(ifelse(HasValue(object@PUAAmt), object@PUAAmt, 0))
   }
)

setMethod(
   f = "SetPUAAmt<-",
   signature = "Cov",
   definition = function(object, value) {
      object@PUAAmt <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAccBal",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@AccBal)) {
         return(object@AccBal)
      } else {
         return(0)
      }
   }
)
setMethod(
   f = "SetAccBal<-",
   signature = "Cov",
   definition = function(object, value) {
      object@AccBal <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass1",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass1)
   }
)

setMethod(
   f = "SetReportClass1<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass1 <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCovMonths",
   signature = "Cov",
   definition = function(object) {
      return(GetCovMonths(GetPlan(object), object))
   }
)

setMethod(
   f = "GetPremMonths",
   signature = "Cov",
   definition = function(object) {
      return(GetPremMonths(GetPlan(object), object))
   }
)

setMethod(
   f = "GetCovYears",
   signature = "Cov",
   definition = function(object) {
      return(GetCovYears(GetPlan(object), object))
   }
)

setMethod(
   f = "GetPremYears",
   signature = "Cov",
   definition = function(object) {
      return(GetPremYears(GetPlan(object), object))
   }
)

setMethod(
   f = "Project",
   signature = "Cov",
   definition = function(object, resultContainer = list()) {
      return(Project(GetPlan(object), object, resultContainer))
   }
)

setMethod(
   f = "GetExpiryDate",
   signature = "Cov",
   definition = function(object) {
      plan <- GetPlan(object)
      return(GetExpiryDate(plan, object))
   }
)

# Helper function to convert data.frame to CovData
as.CovList <- function(dfCov) {
   stopifnot(is.data.frame(dfCov))
   if (nrow(dfCov) == 0) {return(list())}
   fieldList <- colnames(dfCov)
   slotList <- slotNames(new("Cov"))
   slotsWithData <- slotList[which(slotList %in% fieldList)]
   covList <- lapply(
      X = 1:nrow(dfCov),
      FUN = function(r) {
         cov <- new(Class = "Cov")
         for (s in slotsWithData) {
            slot(cov, s) <- switch (class(slot(cov, s)),
                                    "integer" = as.integer(dfCov[[r, s]]),
                                    "S4" = as.Date(dfCov[[r, s]]),
                                    dfCov[[r, s]]
            )
         }
         return(cov)
      }
   )
   return(covList)
}

GetCovLifeInfo <- function(cov, n) {
   SetIssAge(cov) <- GetIssAge(cov)[n]
   SetRiskClass(cov) <- GetRiskClass(cov)[n]
   return(cov)
}

IsSingleLife <- function(cov) {
   return(length(GetIssAge(cov)) == 1 & length(GetRiskClass) == 1)
}


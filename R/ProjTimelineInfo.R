setClass(
   Class = "ProjTimelineInfo",
   slots = c(
      ProjTimeline = "Date",
      CovTimeline = "Date",
      CovProjTimeIndex = "numeric",
      CovProjLen = "numeric",
      ProjLen = "numeric",
      ProjPolMonths = "numeric"
   )
)


# Consturctor: ProjTimelineInfo
ProjTimelineInfo <- function(projStartDate, cov, plan = GetPlan(cov)) {
   if (projStartDate > GetExpiryDate(plan, cov)){
      return(NULL)
   }
   covMonths <- GetCovMonths(plan, cov)
   covTimeline <- GetIssDate(cov) %m+% months(0:(covMonths))
   covProjTimeIndex <- lubridate::interval(projStartDate, covTimeline) / months(1)
   # covProjLen <- sum(covProjTimeIndex >= 0)
   # projLen <- covProjLen + ifelse(GetIssDate(cov) > projStartDate, floor(covProjTimeIndex[1]), 0)
   # projTimeline <- projStartDate %m+% months(0:(projLen - 1))
   covProjLen <- sum(covProjTimeIndex > -1) - 1
   projLen <- covProjLen + ifelse(GetIssDate(cov) > projStartDate, ceiling(covProjTimeIndex[1]), 0)
   projTimeline <- projStartDate %m+% months(0:(projLen - 1))
   projPolMonths = rev((covMonths:1)[1:covProjLen])
   info <- new(
      Class = "ProjTimelineInfo",
      CovTimeline = covTimeline,
      CovProjTimeIndex = covProjTimeIndex,
      ProjTimeline = projTimeline,
      CovProjLen = covProjLen,
      ProjLen = projLen,
      ProjPolMonths = projPolMonths
   )
   return(info)
}


# Another way to create an instance of ProjTimelineInfo object.  Equivalent to constructor ProjTimelineInfo.
GetProjTimelineInfo <- function(projStartDate, cov, plan = GetPlan(cov)) {
   return(ProjTimelineInfo(projStartDate, cov, plan))
}


setMethod(
   f = "GetProjTimeline",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@ProjTimeline)
   }
)


setMethod(
   f = "GetCovTimeline",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@CovTimeline)
   }
)


setMethod(
   f = "GetCovProjTimeIndex",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@CovProjTimeIndex)
   }
)


setMethod(
   f = "GetProjStartDate",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@ProjTimeline[1])
   }
)


setMethod(
   f = "GetCovProjTimeline",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@CovTimeline[object@CovProjTimeIndex >= 0])
   }
)



setMethod(
   f = "GetProjLen",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      #return(length(object@ProjTimeline))
      return(object@ProjLen)
   }
)


setMethod(
   f = "GetCovProjLen",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      # return(sum(object@CovProjTimeIndex >= 0))
      return(object@CovProjLen)
   }
)


setMethod(
   f = "GetProjTimeLabel",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      t <- GetProjTimeline(object)
      tLabel <- paste0(year(t), "-", sprintf("%02d",month(t)))
      return(tLabel)
   }
)


setMethod(
   f = "GetCovProjTimeLabel",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      t <- GetCovProjTimeline(object)
      tLabel <- paste0(year(t), "-", sprintf("%02d",month(t)))
      return(tLabel)
   }
)

setGeneric(name = "GetProjPolMonths", def = function(object, ...) {standardGeneric("GetProjPolMonths")})

setMethod(
   f = "GetProjPolMonths",
   signature = "ProjTimelineInfo",
   definition = function(object) {
      return(object@ProjPolMonths)
   }
)

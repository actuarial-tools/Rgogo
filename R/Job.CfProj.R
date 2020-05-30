#' @include IJob.R
NULL

setClass(
   Class = "Job.CfProj",
   contains = "IJob",
   slots = c(
      ProjStartDate = "Date",
      ProjYears = "numeric",
      Annualized = "logical"
   )
)


setValidity(
   Class = "Job.CfProj",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(Validator.Range(minValue = 1, maxValue = 200), object@ProjYears)
      if (isValid != TRUE) {
         AddMessage(err) <- "Value of slot '@ProjYears' must be between 1 and 200."
      }
      isValid <- Validate(Validator.Length(minLen = 1, maxLen = 1), object@Annualized)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot '@Annualized' must contain a logic scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


Job.CfProj <- function(inpVars = list(), dispatcher = character(), projStartDate = as.Date("1900-01-01"), projYears = 50, annualized = TRUE) {
   job <- new(
      Class = "Job.CfProj",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      ProjStartDate = projStartDate,
      ProjYears = projYears,
      Annualized = annualized
   )
   return(job)
}


setMethod(
   f = "GetProjYears",
   signature = "Job.CfProj",
   definition = function(object) {
      return(object@ProjYears)
   }
)


setMethod(
   f = "SetProjYears<-",
   signature = "Job.CfProj",
   definition = function(object, value) {
      object@ProjYears <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetProjMonths",
   signature = "Job.CfProj",
   definition = function(object) {
      projLen <- round(GetProjYears(object) * 12, digits = 0)
      return(projLen)
   }
)


setMethod(
   f = "GetProjStartDate",
   signature = "Job.CfProj",
   definition = function(object) {
      return(object@ProjStartDate)
   }
)

setMethod(
   f = "SetProjStartDate<-",
   signature = "Job.CfProj",
   definition = function(object, value) {
      object@ProjStartDate <- as.Date(value)
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "IsAnnualized",
   signature = "Job.CfProj",
   definition = function(object) {
      return(object@Annualized)
   }
)


setMethod(
   f = "IsAnnualized<-",
   signature = "Job.CfProj",
   definition = function(object, value) {
      object@Annualized <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "Initialize",
   signature = "Job.CfProj",
   definition = function(object) {
      return(object)     # Do nothing.
   }
)


setMethod(
   f = "Finalize",
   signature = "Job.CfProj",
   definition = function(object, jobResult) {
      cfResult <- lapply(
         X = jobResult,
         FUN = function(result, job) {
            if (IsAnnualized(object)) {
               dfCf <- as.data.frame(lapply(result$Cf, function(cf) {if (is.numeric(cf)) return(GetYearlyTotal(cf)) else return(GetYearStartValue(cf))}))
            } else {
               dfCf <- result$Cf
            }
            return(dfCf)
         }, object
      )
      s <- paste0("rbind(", paste0(paste0("cfResult[[", 1:length(cfResult), "]]"), collapse = ","), ")")
      cfResult <- eval(expr = parse(text = s))
      if (IsAnnualized(object)) {
         timeLabel <- GetProjStartDate(object) %m+% months((0:ceiling(GetProjYears(object))) * 12)
      } else {
         timeLabel <- GetProjStartDate(object) %m+% months(0:GetProjMonths(object))
      }
      timeLabel <- paste0(lubridate::year(timeLabel), "-", sprintf("%02d",lubridate::month(timeLabel)))
      s1 <- paste0("Timeline = c(", paste0("'", timeLabel, "'", collapse = ","), "), ")
      cfNames <- names(cfResult)[2:dim(cfResult)[2]]
      s2 <- paste0(cfNames, " = rep(0, length.out = length(timeLabel))", collapse = ",")
      df <- eval(expr = parse(text = paste0("data.frame(", s1, s2, ", stringsAsFactors = FALSE)")))
      dfCf <- rbind(df, cfResult[(cfResult[,1] %in% timeLabel),])
      s2 <- paste0(cfNames, " = sum(", cfNames, ")", collapse = ",")
      dfCf <- eval(expr = parse(text = paste0("dfCf %>% dplyr::group_by(Timeline) %>% dplyr::summarize(", s2, ")")))
      cnames <- names(dfCf)
      for (cname in cnames) {
         if (all(dfCf[, cname] == 0)) {
            dfCf[, cname] <- NULL
         }
      }
      jobResult <- list(Cf = dfCf)
      if (object@ExportExcel != FALSE) {
         ExportToExcel(object, jobResult)
      }
      return(jobResult)
   }
)


setMethod(
   f = "ExportToExcel",
   signature = "Job.CfProj",
   definition = function(object, result) {
      if (object@ExportExcel != FALSE) {
         wb <- openxlsx::createWorkbook()
         sht <- openxlsx::addWorksheet(wb, sheetName = "Cf")
         openxlsx::writeDataTable(wb, sht, result[[1]])
         openxlsx::saveWorkbook(wb, object@ExportExcel, overwrite = TRUE)
     }
   }
)






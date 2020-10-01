setClass(
   Class = "Job.Valuation",
   contains = "IJob",
   slots = c(
      MaxProjYears = "integer",
      DbAppend = "logical"
   )
)

Job.Valuation <- function(inpVars, dispatcher, dbDrvr, dbConnArgs, maxProjYears = 20L, dbAppend = FALSE, id, descrip = character(0L)) {
   job <- new(
      Class = "Job.Valuation",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      MaxProjYears = as.integer(maxProjYears),
      DbAppend = dbAppend,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setValidity(
   Class = "Job.Valuation",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = 100)
         ),
         object@MaxProjYears
      )
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetMaxProjYears",
   signature = "Job.Valuation",
   definition = function(object) {
      return(object@MaxProjYears)
   }
)

setMethod(
   f = "SetMaxProjYears<-",
   signature = "Job.Valuation",
   definition = function(object, value) {
      object@MaxProjYears <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "Initialize",
   signature = "Job.Valuation",
   definition = function(object) {
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         if (object@DbAppend == FALSE) {
            whereClause <- paste0("JobId = '", GetId(object), "'")
            DeleteRows(conn, "ValuSumm", whereClause)
            DeleteRows(conn, "Cf", whereClause)
         }
         DisconnectDb(conn)
      }
      return(object)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.Valuation",
   definition = function(object, result, digits = 0) {
      jobId <- GetJobId(object)
      valuSumm <- cbind(JobId = jobId, To.data.frame(result, "ValuSumm"))
      cf <- cbind(JobId = jobId, To.data.frame(result, "Cf"))
      # Output job results
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.ValuSumm(conn, valuSumm)
         if (!is.null(cf)) {
            WriteTable.Cf(conn, cf)
         }
         CompactDb(conn)
         DisconnectDb(conn)
      }
      return(list(ValuSumm = valuSumm, Cf = cf))
   }
)

ExportToExcel.Job.Valuation <- function(result, dir, annualized = TRUE, digits = 0, overwrite = FALSE) {
   lst <- lapply(result,
                 function(rslt, d, anlz, dgt, ow) {ExportToExcel.Model.PPM(rslt, d, anlz, dgt, ow)},
                 dir, annualized, digits, overwrite)
   n <- sum(unlist(lst))
   return(cat("Number of Excel files created:", n))
}





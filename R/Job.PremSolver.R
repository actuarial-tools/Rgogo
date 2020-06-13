setClass(
   Class = "Job.PremSolver",
   contains = "IJob"
)

Job.PremSolver <- function(inpVars = list(), dispatcher = character(),
                           exportToRda = FALSE, exportToExcel = FALSE,
                           id = character(0L), descrip = character(0L)) {
   job <- new(
      Class = "Job.PremSolver",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      ExportRda = exportToRda,
      ExportExcel = exportToExcel,
      Id = id,
      Descrip = descrip
   )
   return(job)
}

setMethod(
   f = "Finalize",
   signature = "Job.PremSolver",
   definition = function(object, jobResult) {
      jobResult <- unlist(jobResult)
      if (object@ExportRda != FALSE) {
         for (tbl in jobResult) {
            SaveAsRda(tbl, overwrite = TRUE)
         }
      }
      if (object@ExportExcel != FALSE) {
         ExportToExcel(object, jobResult)
      }
      return(jobResult)
   }
)

setMethod(
   f = "ExportToExcel",
   signature = "Job.PremSolver",
   definition = function(object, result) {
      if (object@ExportExcel != FALSE) {
         wb <- openxlsx::createWorkbook()
         shtName <- ifelse(length(GetId(job)) > 0, GetId(job), class(job))
         length(shtName) <- min(length(shtName), 31)    # 31 is the maximum length of worksheet name in Excel
         sht <- openxlsx::addWorksheet(wb, sheetName = shtName)
         scol <- 1
         for (tbl in result) {
            openxlsx::writeData(wb, sheet = shtName, startCol = scol, startRow = 1, x = GetId(tbl))
            tblValue <- as.data.frame(tbl@TValue)
            openxlsx::writeDataTable(wb, sheet = 1, startCol = scol, startRow = 2, rowNames = TRUE, x = tblValue)
            scol <- scol + 3
         }
         openxlsx::saveWorkbook(wb, file = object@ExportExcel, overwrite = TRUE)
      }
   }
)



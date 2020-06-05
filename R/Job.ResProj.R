

# setGeneric(name = "GetExcelPath", def = function(object) {standardGeneric("GetExcelPath")})
# setGeneric(name = "SetExcelPath<-", def = function(object, value) {standardGeneric("SetExcelPath<-")})


setClass(
   Class = "Job.ResProj",
   contains = "IJob"
)


Job.ResProj <- function(inpVars = list(), dispatcher = character()) {
   job <- new(
      Class = "Job.ResProj",
      InpVars = inpVars,
      Dispatcher = dispatcher
   )
   return(job)
}


# setMethod(
#    f = "GetExcelPath",
#    signature = "Job.ResProj",
#    definition = function(object) {
#       return(object@ExcelPath)
#    }
# )


# setMethod(
#    f = "SetExcelPath<-",
#    signature = "Job.ResProj",
#    definition = function(object, value) {
#       if (length(value) > 0) {
#          if (!endsWith(value, ".xlsx")) {
#             value <- paste0(value, ".xlsx")
#             warning("'.xlsx' is appended to the Excel file path.")
#          }
#          if (file.exists(value)) {
#             warning(paste0("File '", value, "' already exists. It will be overwritten."))
#          }
#       }
#       object@ExcelPath <- value
#       return(object)
#    }
# )


setMethod(
   f = "Finalize",
   signature = "Job.ResProj",
   definition = function(object, jobResult) {
      s <- paste0("rbind(", paste0(paste0("jobResult[[", 1:length(jobResult), "]]$Res"), collapse = ","), ")")
      resProj <- eval(expr = parse(text = s))
      resProj %<>%
         dplyr::group_by(ValuDate) %>%
         dplyr::summarise(
            Res.Gross = sum(Res.Gross),
            Res.Rein = sum(Res.Rein),
            Res.Net = sum(Res.Net)
         )
      jobResult$ResProj <- resProj
      if (object@ExportExcel != FALSE) {
         ExportToExcel(object, jobResult)
      }
      return(jobResult)
   }
)


# setMethod(
#    f = "ExportToExcel",
#    signature = "Job.ResProj",
#    definition = function(object, path, result, digits = 0) {
#       dataTable <- Round.data.frame(result$ResProj, digits)
#       wb <- openxlsx::createWorkbook()
#       shtName <- "ResProj"
#       openxlsx::addWorksheet(wb, sheetName = shtName)
#       openxlsx::writeDataTable(wb, sheet = shtName, x = dataTable, startCol = 1, startRow = 1)
#       openxlsx::setColWidths(wb, sheet = shtName, cols = 1:dim(dataTable)[2], widths = 12)
#       openxlsx::saveWorkbook(wb, file = GetExcelPath(object), overwrite = TRUE)
#    }
# )


setMethod(
   f = "ExportToExcel",
   signature = "Job.ResProj",
   definition = function(object, result) {
      if (object@ExportExcel != FALSE) {
         df <- Round.data.frame(result$ResProj, digits = 0)
         wb <- openxlsx::createWorkbook()
         sht <- openxlsx::addWorksheet(wb, sheetName = "ResProj")
         openxlsx::writeDataTable(wb, sht, df, startCol = 1, startRow = 1)
         openxlsx::setColWidths(wb, sht, cols = 1:dim(df)[2], widths = 10)
         openxlsx::saveWorkbook(wb, object@ExportExcel, overwrite = TRUE)
      }
   }
)



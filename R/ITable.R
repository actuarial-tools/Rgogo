setClass(Class = "ITable",
         contains = c("IObject", "VIRTUAL"),
         slots = c(Source = "character",
                   CreatedBy = "character",
                   CreatedAt = "POSIXct",
                   TBase = "numeric"
         )
)


setMethod(
   f = "GetSource",
   signature = "ITable",
   definition = function(object) {
      return(object@Source)
   }
)


setMethod(
   f = "SetSource<-",
   signature = c("ITable", "character"),
   definition = function(object, value) {
      object@Source <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetCreatedBy",
   signature = "ITable",
   definition = function(object) {
      return(object@CreatedBy)
   }
)


setMethod(
   f = "SetCreatedBy<-",
   signature = c("ITable", "character"),
   definition = function(object, value) {
      object@CreatedBy <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetCreatedAt",
   signature = "ITable",
   definition = function(object) {
      return(object@CreatedAt)
   }
)


setMethod(
   f = "SetCreatedAt<-",
   signature = c("ITable", "POSIXct"),
   definition = function(object, value) {
      object@CreatedAt <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetTBase",
   signature = "ITable",
   definition = function(object) {
      return(object@TBase)
   }
)


setMethod(
   f = "LookUp",
   signature = "ITable",
   definition = function(tbl, lookUpKey, ...) {
      stop("'LookUp' method must be implemented by a child class of 'ITable'")
   }
)


setMethod(
   f = "SaveAsRda",
   signature = "ITable",
   definition = function(object, overwrite = FALSE, tblType = character()) {
      stopifnot(HasValue(GetId(object)), length(tblType) <= 1)
      id <- GetId(object)
      if (HasValue(tblType)) {
         tblType <- ifelse(endsWith(tblType, "."), tblType, paste0(tblType, "."))
         rdaName <- ifelse(startsWith(id, tblType), id, paste0(tblType, id))
      } else {
         rdaName <- id
      }
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)


setMethod(
   f = "ExportToExcel",
   signature = "ITable",
   definition = function(object, path, overwrite = FALSE) {
      maxShtNameLen <- 30
      wb <- openxlsx::createWorkbook()
      shtName <- substr(GetId(object), 1, maxShtNameLen)
      headerStyle <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold")
      tblDescripStyle <- openxlsx::createStyle(halign = "left")
      footerStyle <- openxlsx::createStyle(fontSize = 10, textDecoration = "italic")
      tblValueColWidth <- 10
      r <- 0
      openxlsx::addWorksheet(wb, sheetName = shtName)
      r <- r + 1
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = ifelse(HasValue(GetDescrip(object)), GetDescrip(object), GetId(object)))
      openxlsx::addStyle(wb, shtName, style = headerStyle, rows = r, cols = 1)
      r <- r + 2
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = "Table Id: ")
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 2, x = GetId(object))
      openxlsx::addStyle(wb, shtName, style = tblDescripStyle, rows = r, cols = 2)
      r <- r + 1
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = "Source:")
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 2, x = GetSource(object))
      openxlsx::addStyle(wb, shtName, style = tblDescripStyle, rows = r, cols = 2)
      r <- r + 1
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = "Base:")
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 2, x = GetTBase(object))
      openxlsx::addStyle(wb, shtName, style = tblDescripStyle, rows = r, cols = 2)
      r <- r + 2
      output <- .ExportToExcel.TValue(object, wb, shtName, startRow = r, startCol = 1, colWidth = tblValueColWidth)
      wb <- output$Workbook
      r <- r + output$RowCount + 2
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = paste0("Table object created by: ", GetCreatedBy(object)))
      openxlsx::addStyle(wb, shtName, style = footerStyle, rows = r, cols = 1)
      r <- r + 2
      openxlsx::writeData(wb = wb, sheet = shtName, startRow = r, startCol = 1, x = lubridate::stamp("Created at: 2020-01-01 12:00:00", quiet = TRUE)(GetCreatedAt(object)))
      openxlsx::addStyle(wb, shtName, style = footerStyle, rows = r, cols = 1)
      openxlsx::saveWorkbook(wb = wb, file = path, overwrite = overwrite)
   }
)



# Helper function to read select and ultimate table values from Excel file.
ImportTableValuesFromExcel.SU <- function(tbl, excelFileName, sheet, selValueRange, ultValueRange, transposeData = FALSE) {
   stopifnot(class(tbl) == "Table.SU", file.exists(excelFileName))
   # Suppress messages because read_excel will generate unwanted messages when setting col_names = FALSE
   suppressMessages(selTable <- as.matrix(readxl::read_excel(path = excelFileName, sheet = sheet, range = selValueRange, col_names = FALSE)))
   if(transposeData) selTable <- t(selTable)
   suppressMessages(ultTable <- as.matrix(readxl::read_excel(path = excelFileName, sheet = sheet, range = ultValueRange, col_names = FALSE)))
   dimnames(selTable) <- dimnames(tbl@TValue)
   dimnames(ultTable) <- dimnames(tbl@TValueUlt)
   tbl@TValue <- selTable
   tbl@TValueUlt <- ultTable
   return(tbl)
}


# Helper function to read table values from Excel file.
ImportTableValuesFromExcel <- function(tbl, excelFileName, sheet, valueRange1, ..., transposeData = FALSE) {
   tblValue <- matrix(nrow = nrow(tbl@TValue), ncol = ncol(tbl@TValue))
   valueRanges <- c(valueRange1, unlist(list(...)))
   if(length(valueRanges) >1 ){
      for (i in (1:ncol(tblValue))) {
         suppressMessages(data <- as.matrix(readxl::read_excel(path = excelFileName, sheet = sheet, range = valueRanges[i], col_names = FALSE)))
         if(transposeData) tblValue[,i] <- t(data) else tblValue[,i] <- data
      }
   } else {
      suppressMessages(data <- as.matrix(readxl::read_excel(path = excelFileName, sheet = sheet, range = valueRanges[1], col_names = FALSE)))
      if(transposeData) tblValue <- t(data) else tblValue <- data
   }
   rowsCopied <- 1:min(dim(tblValue)[1], dim(tbl@TValue[1]))
   colsCopied <- 1:min(dim(tblValue)[2], dim(tbl@TValue[2]))
   tbl@TValue[rowsCopied, colsCopied] <- tblValue[rowsCopied, colsCopied]
   return(tbl)
}



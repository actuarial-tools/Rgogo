# Import tabular data from Excel and return a data.frame according to specified data.frame schema.
ImportTabularDataFromExcel <- function(excelFile, sheet, outputSchema) {
   excelData <- readxl::read_excel(path = excelFile, sheet = sheet)
   recCnt <- nrow(excelData)
   excelDataColNames <- names(excelData)
   excelDataColNum <- match(outputSchema$ColumnName, excelDataColNames)
   argString <- ""
   for (r in 1:nrow(outputSchema)) {
      if (is.na(excelDataColNum[r])) {
         argString <- paste0(argString, outputSchema$ColumnName[[r]], " = ",
                             "rep(switch(outputSchema$DataType[[r]], character = NA_character_, Date = as.Date('1900-01-01'),integer = NA_integer_, numeric = NA_real_), length.out = ", recCnt, "), ")
      } else {
         argString <- paste0(argString, outputSchema$ColumnName[[r]], " = ",
                             switch(outputSchema$DataType[[r]], character = "as.character(", Date = "as.Date(",
                                    integer = "as.integer(", numeric = "as.numeric("), "excelData$", excelDataColNames[excelDataColNum[r]], "), ")
      }
   }
   argString <- paste0("data.frame(", argString, "stringsAsFactors = FALSE)")
   df <- eval(expr = parse(text = argString))
   return(df)
}



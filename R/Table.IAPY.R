#' @include ITable.R
NULL


setClass(Class = "Table.IAPY",
         contains = "ITable",
         slots = c(MinAge = "integer",
                   MaxAge = "integer",
                   MaxPolYear = "integer",
                   TValue = "matrix"
         )
)


Table.IAPY <- function(minAge, maxAge, maxPolYear, tBase){
   tbl <- new(Class = "Table.IAPY")
   tbl@TValue <- matrix(nrow = maxAge - minAge + 1,
                        ncol = maxPolYear,
                        dimnames = list(as.character(minAge:maxAge), as.character(1:maxPolYear))
   )
   tbl@MinAge <- as.integer(minAge)
   tbl@MaxAge <- as.integer(maxAge)
   tbl@MaxPolYear <- as.integer(maxPolYear)
   tbl@TBase <- as.numeric(tBase)
   tbl@CreatedAt <- Sys.time()
   return(tbl)
}


setMethod(
   f = "GetMinAge",
   signature = "Table.IAPY",
   definition = function(object) {
      return(object@MinAge)
   }
)


setMethod(
   f = "GetMaxAge",
   signature = "Table.IAPY",
   definition = function(object) {
      return(object@MaxAge)
   }
)


setMethod(
   f = "GetMaxPolYear",
   signature = "Table.IAPY",
   definition = function(object) {
      return(object@MaxPolYear)
   }
)


setMethod(
   f = "LookUp",
   signature (tbl = "Table.IAPY", lookUpKey = "list"),
   definition = function(tbl, lookUpKey) {
      stopifnot(HasValue(lookUpKey$IssAge), HasValue(lookUpKey$PolYear))
      issAge <- as.character(lookUpKey$IssAge)
      polYear <- as.character(lookUpKey$PolYear)
      stopifnot(all(issAge %in% dimnames(tbl@TValue)[[1]]))
      stopifnot(all(polYear %in% dimnames(tbl@TValue)[[2]]))
      v <- tbl@TValue[issAge, polYear] / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)


setMethod(
   f = "LookUp",
   signature (tbl = "Table.IAPY", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      issAge <- as.character(GetIssAge(lookUpKey))
      if (is.na(len)) {
         polYear <- as.character(1:GetMaxPolYear(tbl))
      } else {
         polYear <- as.character(1:len)
      }
      stopifnot(all(issAge %in% dimnames(tbl@TValue)[[1]]))
      stopifnot(all(polYear %in% dimnames(tbl@TValue)[[2]]))
      v <- tbl@TValue[issAge, polYear] / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)


setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.IAPY",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      tbl <- as.data.frame(object@TValue, stringsAsFactors = FALSE)
      tbl <- cbind(rownames(object@TValue), tbl)
      colnames(tbl) <- c("IssueAge", paste0("PY",sprintf("%03d", 1:object@MaxPolYear)))
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      openxlsx::setColWidths(wb, sheet, cols = 1:dim(tbl)[2], widths = colWidth)
      return(list(Workbook = wb, RowCount = dim(tbl)[1] + 1, ColCount = dim(tbl)[2]))
   }
)





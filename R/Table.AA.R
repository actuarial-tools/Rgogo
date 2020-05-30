#' @include ITable.R
NULL


# Class definition
setClass(Class = "Table.AA",
         contains = "ITable",
         slots = c(MinAge = "integer",
                   MaxAge = "integer",
                   TValue = "matrix"
         )
)


# Constructor
Table.AA <- function(minAge, maxAge, tBase) {
   tbl <- new(Class = "Table.AA")
   tbl@TValue <- matrix(nrow = maxAge - minAge + 1,
                        ncol = 1,
                        dimnames = list(as.character(minAge:maxAge), NULL)
   )
   tbl@MinAge <- as.integer(minAge)
   tbl@MaxAge <- as.integer(maxAge)
   tbl@TBase <- as.numeric(tBase)
   tbl@CreatedAt <- Sys.time()
   return(tbl)
}


setMethod(
   f = "GetMinAge",
   signature = "Table.AA",
   definition = function(object) {
      return(object@MinAge)
   }
)


setMethod(
   f = "GetMaxAge",
   signature = "Table.AA",
   definition = function(object) {
      return(object@MaxAge)
   }
)


setMethod(
   f = "LookUp",
   signature (tbl = "Table.AA", lookUpKey = "list"),
   definition = function(tbl, lookUpKey) {
      stopifnot(HasValue(attAge <- lookUpKey$AttAge))
      attAge <- as.character(attAge)
      stopifnot(all(attAge %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[attAge,1]) / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)


setMethod(
   f = "LookUp",
   signature (tbl = "Table.AA", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      issAge <- GetIssAge(lookUpKey)
      if (is.na(len)) {
         attAge <- as.character(issAge:GetMaxAge(tbl))
      } else {
         attAge <- as.character(issAge:(issAge + len - 1))
      }
      stopifnot(all(attAge %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[attAge,1]) / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)


setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.AA",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      tbl <- data.frame(
         AttAge = rownames(object@TValue),
         Value = object@TValue[,1],
         stringsAsFactors = FALSE
      )
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      openxlsx::setColWidths(wb, sheet, cols = 1:2, widths = colWidth)
      return(list(Workbook = wb, RowCount = dim(tbl)[1] + 1, ColCount = dim(tbl)[2]))
   }
)





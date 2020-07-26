#' @include ITable.R
NULL

setClass(Class = "Table.AA",
         contains = "ITable",
         slots = c(MinAge = "integer",
                   MaxAge = "integer",
                   TValue = "matrix"
         )
)

setValidity(
   Class = "Table.AA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @MinAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@MinAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MinAge: Minimum age must be an integer and cannot be negative (@MinAge)."
      }
      # Validate @MaxAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = object@MinAge)
         ),
         object@MaxAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxAge: Maximum age must be an integer and cannot be less than the minimum age (@MaxAge)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Table.AA <- function(minAge, maxAge, tBase, tValue = NA,
                     source = character(0L), createdBy = character(0L),
                     id = character(0L), descrip = character(0L)) {
   tbl <- new(Class = "Table.AA")
   tbl@TValue <- matrix(data = tValue,
                        nrow = maxAge - minAge + 1,
                        ncol = 1,
                        dimnames = list(as.character(minAge:maxAge), NULL)
   )
   tbl@MinAge <- as.integer(minAge)
   tbl@MaxAge <- as.integer(maxAge)
   tbl@TBase <- as.numeric(tBase)
   tbl@Source <- as.character(source)
   tbl@CreatedBy <- as.character(createdBy)
   tbl@CreatedAt <- Sys.time()
   tbl@Id <- as.character(id)
   tbl@Descrip <- as.character(descrip)
   validObject(tbl)
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





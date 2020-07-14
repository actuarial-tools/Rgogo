#' @include ITable.R
NULL

setClass(Class = "Table.AABY",
         contains = "ITable",
         slots = c(MinAge = "integer",
                   MaxAge = "integer",
                   MinYear = "integer",
                   MaxYear = "integer",
                   TValue = "matrix"
         )
)

setValidity(
   Class = "Table.AABY",
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
      # Validate @MinYear
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 1800, maxValue = 9999)
         ),
         object@MinYear
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MinYear: Minimum year must be an integer between 1800 and 9999 (@MinYear)."
      }
      # Validate @MaxYear
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = object@MinYear, maxValue = 9999)
         ),
         object@MaxYear
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxYear: Maximum year must be an integer between minimum year and 9999 (@MinYear)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Table.AABY <- function(minAge, maxAge, minBirthYear, maxBirthYear, tBase, tValue = NA, fillByAge = TRUE){
   stopifnot(minBirthYear >= 1900, minBirthYear <= 9999, maxBirthYear >= 1900, maxBirthYear <= 9999, minBirthYear <= maxBirthYear)
   tbl <- new(Class = "Table.AABY")
   tbl@TValue <- matrix(data = tValue,
                        nrow = maxAge - minAge + 1,
                        ncol = maxBirthYear - minBirthYear + 1,
                        dimnames = list(as.character(minAge:maxAge), as.character(minBirthYear:maxBirthYear)),
                        byrow = fillByAge
   )
   tbl@MinAge <- as.integer(minAge)
   tbl@MaxAge <- as.integer(maxAge)
   tbl@MinYear <- as.integer(minBirthYear)
   tbl@MaxYear <- as.integer(maxBirthYear)
   tbl@TBase <- as.numeric(tBase)
   tbl@CreatedAt <- Sys.time()
   return(tbl)
}

setMethod(
   f = "GetMinAge",
   signature = "Table.AABY",
   definition = function(object) {
      return(object@MinAge)
   }
)

setMethod(
   f = "GetMaxAge",
   signature = "Table.AABY",
   definition = function(object) {
      return(object@MaxAge)
   }
)

setMethod(
   f = "GetMinBirthYear",
   signature = "Table.AABY",
   definition = function(object) {
      return(object@MinYear)
   }
)

setMethod(
   f = "GetMaxBirthYear",
   signature = "Table.AABY",
   definition = function(object) {
      return(object@MaxYear)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.AABY", lookUpKey = "list"),
   definition = function(tbl, lookUpKey){
      stopifnot(HasValue(lookUpKey$AttAge), HasValue(lookUpKey$BirthYear))
      attAge <- as.character(lookUpKey$AttAge)
      birthYear <- as.character(lookUpKey$BirthYear)
      stopifnot(all(attAge %in% dimnames(tbl@TValue)[[1]]))
      stopifnot(all(birthYear %in% dimnames(tbl@TValue)[[2]]))
      v <- tbl@TValue[attAge, birthYear] / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.AABY", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      issAge <- GetIssAge(lookUpKey)
      birthYear <- as.character(as.numeric(format(GetIssDate(lookUpKey), "%Y")) - issAge)
      if (is.na(len)) {
         attAge <- as.character(issAge:GetMaxAge(tbl))
      } else {
         attAge <- as.character(issAge:(issAge + len - 1))
      }
      stopifnot(all(attAge %in% dimnames(tbl@TValue)[[1]]))
      stopifnot(all(birthYear %in% dimnames(tbl@TValue)[[2]]))
      v <- tbl@TValue[attAge, birthYear] / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.AABY",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      tbl <- as.data.frame(object@TValue, stringsAsFactors = FALSE)
      tbl <- cbind(rownames(object@TValue), tbl)
      colnames(tbl) <- c("AttAge", paste0("BY",sprintf("%04d", GetMinBirthYear(object):GetMaxBirthYear(object))))
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      openxlsx::setColWidths(wb, sheet, cols = 1:dim(tbl)[2], widths = colWidth)
      return(list(Workbook = wb, RowCount = dim(tbl)[1] + 1, ColCount = dim(tbl)[2]))
   }
)


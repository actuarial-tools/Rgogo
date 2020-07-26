#' @include ITable.R
NULL

setClass(Class = "Table.SU",
         contains = "ITable",
         slots = c(MinSelAge = "integer",
                   MaxSelAge = "integer",
                   SelPeriod = "integer",
                   MaxAttAge = "integer",
                   TValue = "matrix",
                   TValueUlt = "matrix"
         )
)

setValidity(
   Class = "Table.SU",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @MinSelAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@MinSelAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MinSelAge: Minimum select age must be an integer and cannot be negative (@MinSelAge)."
      }
      # Validate @MaxSelAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = object@MinSelAge)
         ),
         object@MaxSelAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxSelAge: Maximum select age must be an integer and cannot be less than the minimum age (@MaxSelAge)."
      }
      # Validate @SelPeriod
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 1)
         ),
         object@SelPeriod
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxAttAge: Invalid select period (@SelPeriod).  Must be a positive integer."
      }
      # Validate @MaxAttAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = object@MaxSelAge)
         ),
         object@MaxAttAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxAttAge: Invalid maximum attained age (@MaxAttAge)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Table.SU <- function(minSelAge, maxSelAge, selPeriod, maxAttAge, tBase, tValueSel = NA, fillByAge = TRUE, tValueUlt = NA,
                     source = character(0L), createdBy = character(0L),
                     id = character(0L), descrip = character(0L)) {
   tbl <- new(Class = "Table.SU")
   tbl@TValue <- matrix(data = tValueSel,
                        nrow = maxSelAge - minSelAge + 1,
                        ncol = selPeriod,
                        dimnames = list(as.character(minSelAge:maxSelAge), as.character(1:selPeriod)),
                        byrow = fillByAge
   )
   tbl@TValueUlt <- matrix(data = tValueUlt,
                           nrow = maxAttAge - (minSelAge + selPeriod) + 1,
                           ncol = 1,
                           dimnames = list((as.character((minSelAge + selPeriod): maxAttAge)), "Ult")
   )
   tbl@MinSelAge <- as.integer(minSelAge)
   tbl@MaxSelAge <- as.integer(maxSelAge)
   tbl@SelPeriod <- as.integer(selPeriod)
   tbl@MaxAttAge <- as.integer(maxAttAge)
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
   f = "GetMinSelAge",
   signature = "Table.SU",
   definition = function(object) {
      return(object@MinSelAge)
   }
)

setMethod(
   f = "GetMaxSelAge",
   signature = "Table.SU",
   definition = function(object) {
      return(object@MaxSelAge)
   }
)

setMethod(
   f = "GetSelPeriod",
   signature = "Table.SU",
   definition = function(object) {
      return(object@SelPeriod)
   }
)

setMethod(
   f = "GetMaxAttAge",
   signature = "Table.SU",
   definition = function(object) {
      return(object@MaxAttAge)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.SU", lookUpKey = "list"),
   definition = function(tbl, lookUpKey) {
      stopifnot(HasValue(lookUpKey$IssAge))
      issAge <- lookUpKey$IssAge
      if (HasValue(lookUpKey$PolYear)) polYear <- lookUpKey$PolYear else polYear <- (1:(GetMaxAttAge(tbl) - min(issAge) + 1))
      vList <- lapply(issAge, .LookUpSelUlt, tbl)
      v <- vList[[1]]
      if (length(vList) == 1) {
         names(v) <- as.character(1:length(v))
         v <- v[as.character(polYear)]
      } else {
         for (i in (2:length(vList))) {
            v <- rbind(v, vList[[i]])
         }
         dimnames(v) <- list(as.character(issAge), as.character(1:ncol(v)))
         v <- v[, as.character(polYear)]
      }
      return(v)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.SU", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      issAge <- GetIssAge(lookUpKey)
      if (is.na(len)) {
         polYear <- 1:(GetMaxAttAge(tbl) - issAge + 1)
      } else {
         polYear <- 1:len
      }
      vList <- lapply(issAge, .LookUpSelUlt, tbl)
      v <- vList[[1]]
      if (length(vList) == 1) {
         names(v) <- as.character(1:length(v))
         v <- v[as.character(polYear)]
      } else {
         for (i in (2:length(vList))) {
            v <- rbind(v, vList[[i]])
         }
         dimnames(v) <- list(as.character(issAge), as.character(1:ncol(v)))
         v <- v[, as.character(polYear)]
      }
      return(v)
   }
)

.LookUpSelUlt <- function(issAge, tbl) {
   maxSelAge <- GetMaxSelAge(tbl)
   selPeriod <- GetSelPeriod(tbl)
   if(issAge <= maxSelAge) {
      sRates <- tbl@TValue[as.character(issAge),]
      uRates <- tbl@TValueUlt[as.integer(rownames(tbl@TValueUlt)) >= (issAge + selPeriod), 1]
   } else {
      sRates <- tbl@TValue[as.character(tbl@MaxSelAge), as.character((issAge - maxSelAge + 1):selPeriod)]
      uRates <- tbl@TValueUlt[as.integer(rownames(tbl@TValueUlt)) >= (maxSelAge + selPeriod), 1]
   }
   v <- c(sRates, uRates) / tbl@TBase
   length(v) <- GetMaxAttAge(tbl) - GetMinSelAge(tbl) + 1
   return(v)
}

setMethod(
   f = "GetUltTable",
   signature = "Table.SU",
   definition = function(object) {
      # Return an instance of Table.AA (attained age table)
      minAge <- GetMinSelAge(object)
      maxAge <- GetMaxAttAge(object)
      tbl <- Table.AA(minAge, maxAge, object@TBase)
      SetId(tbl) <- paste(GetId(object), "UltTable", sep = "_")
      tbl@TValue <- matrix(
         data = c(object@TValue[as.character(minAge),], object@TValueUlt[,1]),
         ncol = 1,
         dimnames = list(as.character(minAge:maxAge), NULL)
      )
      return(tbl)
   }
)

setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.SU",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      # Write select table
      tbl <- as.data.frame(object@TValue, stringsAsFactors = FALSE)
      tbl <- cbind(rownames(object@TValue), tbl)
      colnames(tbl) <- c("SelAge", paste0("PY",sprintf("%02d", 1:GetSelPeriod(object))))
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      # Write ultimate table
      ultTbl <- as.data.frame(object@TValueUlt)
      ultTbl <- cbind(rownames(object@TValueUlt), ultTbl)
      colnames(ultTbl) <- c("AttAge", "UltRate")
      openxlsx::writeDataTable(wb, sheet, startRow = startRow, startCol = startCol + dim(tbl)[2]+ 1, x = ultTbl)
      rowCount <- max(dim(tbl)[1], dim(ultTbl)) + 1
      colCount <- dim(tbl)[2] + dim(ultTbl)[2] + 1
      openxlsx::setColWidths(wb, sheet, cols = 1:colCount, widths = colWidth)
      return(list(Workbook = wb, RowCount = rowCount, ColCount = colCount))
   }
)






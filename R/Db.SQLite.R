setMethod(
   f = "TableExists",
   signature = c("SQLiteConnection", "character"),
   definition = function(conn, tableName) {
      return(DBI::dbExistsTable(conn, tableName))
   }
)

setMethod(
   f = "CreateTable",
   signature = c("SQLiteConnection", "character", "character"),
   definition = function(conn, tableName, dbColSpec, stopIfExists = TRUE, primaryKey = character(0L)) {
      if (stopIfExists & TableExists(conn, tableName)) {
         stop(paste0("A table with the same name '", tableName, "' alread exists."))
      }
      dbColSpec <- paste(names(dbColSpec), dbColSpec)
      sql <- paste("CREATE TABLE", tableName, "(", paste(dbColSpec, collapse = ", "))
      if (length(primaryKey) > 0) {
         sql <- paste(sql, ", PRIMARY KEY (", paste(primaryKey, collapse = ", "), ")")
      }
      sql <- paste(sql, ");")
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "DeleteTable",
   signature = c("SQLiteConnection", "character"),
   definition = function(conn, tableName) {
      sql <- paste("DROP TABLE", tableName, ";")
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "CreateIndex",
   signature = c("SQLiteConnection", "character", "character"),
   definition = function(conn, tableName, indexCol) {
      idxName <- paste0(c("idx", indexCol), collapse = "_")
      sql <- paste0("CREATE INDEX ", idxName, " ON ", tableName, " (", paste0(indexCol, collapse = ", ", ");"))
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "WriteTable",
   signature = c("SQLiteConnection", "character", "data.frame"),
   definition = function(conn, tableName, data, append = TRUE, allowAlterTable = FALSE) {
      DBI::dbWithTransaction(
         conn,
         {
            if (!append) {
               sql <- paste0("DELETE FROM ", tableName, ";")
               DBI::dbExecute(conn, sql)
            }
            dataColNames <- colnames(data)
            newColNames <- dataColNames[!dataColNames %in% DBI::dbListFields(conn, tableName)]
            if (allowAlterTable & length(newColNames) > 0) {
               for (colName in newColNames) {
                  dataType <- eval(expr = parse(text = paste0("DBI::dbDataType(conn, data$", colName, ")")))
                  sql <- paste0("ALTER TABLE ", tableName, " ADD COLUMN ", colName, " ", dataType)
                  DBI::dbExecute(conn, sql)
               }
            }
            DBI::dbWriteTable(conn, "Cov", data, append = TRUE)
         }
      )
   }
)

setMethod(
   f = "ReadTable",
   signature = c("SQLiteConnection", "character"),
   definition = function(conn, tableName, ...) {
      selCond <- list(...)
      sql <- paste0("SELECT * FROM ", tableName)
      if (length(selCond) > 0) {
         whereClause <- paste0(unlist(selCond), collapse = " AND ")
         sql <- paste0(sql, " WHERE ", whereClause)
      }
      sql <- paste0(sql, ";")
      df <- DBI::dbGetQuery(conn, sql)
      return(df)
   }
)

setMethod(
   f = "CreateTable.Cov",
   signature = "SQLiteConnection",
   definition = function(conn) {
      CreateTable(
         conn,
         tableName = "Cov",
         colSpec <- c(
            CovId = DBI::dbDataType(conn, character()),
            PlanId = DBI::dbDataType(conn, character()),
            IssDate = DBI::dbDataType(conn, Sys.Date()),
            IssAge = DBI::dbDataType(conn, integer()),
            RiskClass = DBI::dbDataType(conn, character()),
            FaceAmt = DBI::dbDataType(conn, numeric()),
            PremMode = DBI::dbDataType(conn, integer()),
            ModPrem = DBI::dbDataType(conn, numeric())
         ),
         primaryKey = c("CovId")
      )
      CreateIndex(conn, tableName = "Cov", indexCol = "PlanId")
   }
)

setMethod(
   f = "ReadTable.Cov",
   signature = "SQLiteConnection",
   definition = function(conn, ...) {
      df <- ReadTable(conn, "Cov", ...)
      for (colName in colnames(df)) {
         if(endsWith(colName, "Date")) {
            col <- df[, colName]
            eval(expr = parse(text = paste0("df$", colName, " <- as.Date(col)")))
         }
      }
      return(df)
   }
)

setMethod(
   f = "WriteTable.Cov",
   signature = c("SQLiteConnection", "data.frame"),
   definition = function(conn, data, append = TRUE, allowAlterTable = FALSE) {
      WriteTable(conn, "Cov", data, append, allowAlterTable)
   }
)



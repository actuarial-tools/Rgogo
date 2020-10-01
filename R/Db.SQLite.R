setMethod(
   f = "ConnectDb",
   signature = "DBIDriver",
   definition = function(object, args) {
      return(eval(expr = parse(text = paste0("DBI::dbConnect(object, ", args, ")"))))
   }
)

setMethod(
   f = "ConnectDb",
   signature = "character",
   definition = function(object, args) {
      driver <- eval(expr = parse(text = object))
      return(ConnectDb(driver, args))
   }
)

setMethod(
   f = "DisconnectDb",
   signature = "DBIConnection",
   definition = function(object) {
      DBI::dbDisconnect(object)
   }
)

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
   definition = function(conn, tableName, dbColSpec, primaryKey = character(0L)) {
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
      return(paste0("Table deleted: ", tableName))
   }
)

setMethod(
   f = "CreateIndex",
   signature = c("SQLiteConnection", "character", "character"),
   definition = function(conn, tableName, indexCol) {
      idxName <- paste0(c("idx", tableName, indexCol), collapse = "_")
      sql <- paste0("CREATE INDEX ", idxName, " ON ", tableName, " (", paste0(indexCol, collapse = ", ", ");"))
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "WriteTable",
   signature = c("SQLiteConnection", "character", "ANY"),
   definition = function(conn, tableName, data, append = TRUE, colExpandable = FALSE) {
      DBI::dbWithTransaction(
         conn,
         {
            if (!append) {
               sql <- paste0("DELETE FROM ", tableName, ";")
               DBI::dbExecute(conn, sql)
            }
            dfData <- as.data.frame(data)
            dataColNames <- colnames(dfData)
            newColNames <- dataColNames[!dataColNames %in% DBI::dbListFields(conn, tableName)]
            if (colExpandable & length(newColNames) > 0) {
               for (colName in newColNames) {
                  dataType <- eval(expr = parse(text = paste0("DBI::dbDataType(conn, dfData$", colName, ")")))
                  sql <- paste0("ALTER TABLE ", tableName, " ADD COLUMN ", colName, " ", dataType)
                  DBI::dbExecute(conn, sql)
               }
            }
            DBI::dbWriteTable(conn, tableName, data, append = TRUE)
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
      DBI::dbWithTransaction(
         conn,
         {
            CreateTable(
               conn,
               tableName = "Cov",
               colSpec <- c(
                  Id = DBI::dbDataType(conn, character()),
                  PlanId = DBI::dbDataType(conn, character()),
                  IssDate = DBI::dbDataType(conn, Sys.Date()),
                  IssAge = DBI::dbDataType(conn, integer()),
                  RiskClass = DBI::dbDataType(conn, character()),
                  FaceAmt = DBI::dbDataType(conn, numeric()),
                  PremMode = DBI::dbDataType(conn, integer()),
                  ModPrem = DBI::dbDataType(conn, numeric())
               ),
               primaryKey = c("Id")
            )
            CreateIndex(conn, tableName = "Cov", indexCol = "PlanId")
         }
      )
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
            eval(expr = parse(text = paste0("df$", colName, " <- as.Date('1970-01-01') + lubridate::dseconds(df$", colName, ")")))
         }
      }
      return(df)
   }
)

setMethod(
   f = "WriteTable.Cov",
   signature = c("SQLiteConnection", "ANY"),
   definition = function(conn, data, append = TRUE, colExpandable = FALSE) {
      WriteTable(conn, "Cov", data, append, colExpandable)
   }
)

setMethod(
   f = "CreateTable.ValuSumm",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbWithTransaction(
         conn,
         {
            CreateTable(
               conn,
               tableName = "ValuSumm",
               colSpec <- c(
                  JobId = DBI::dbDataType(conn, character()),
                  CovId = DBI::dbDataType(conn, character()),
                  PlanId = DBI::dbDataType(conn, character()),
                  AnlzPrem = DBI::dbDataType(conn, numeric()),
                  CV = DBI::dbDataType(conn, numeric()),
                  GrossSumInsd = DBI::dbDataType(conn, numeric()),
                  ReinSumInsd = DBI::dbDataType(conn, numeric()),
                  NetSumInsd = DBI::dbDataType(conn, numeric()),
                  GrossRes = DBI::dbDataType(conn, numeric()),
                  ReinRes = DBI::dbDataType(conn, numeric()),
                  NetRes = DBI::dbDataType(conn, numeric()),
                  LiabDur = DBI::dbDataType(conn, numeric()),
                  PVPrem = DBI::dbDataType(conn, numeric()),
                  PVPremTax = DBI::dbDataType(conn, numeric()),
                  PVComm = DBI::dbDataType(conn, numeric()),
                  PVCommOvrd = DBI::dbDataType(conn, numeric()),
                  PVBenDth = DBI::dbDataType(conn, numeric()),
                  PVBenMat = DBI::dbDataType(conn, numeric()),
                  PVBenSur = DBI::dbDataType(conn, numeric()),
                  PVBenAnu = DBI::dbDataType(conn, numeric()),
                  PVBenDthPUA = DBI::dbDataType(conn, numeric()),
                  PVBenMatPUA = DBI::dbDataType(conn, numeric()),
                  PVBenSurPUA = DBI::dbDataType(conn, numeric()),
                  PVExpnsAcq = DBI::dbDataType(conn, numeric()),
                  PVExpnsMnt = DBI::dbDataType(conn, numeric()),
                  PVReinPrem = DBI::dbDataType(conn, numeric()),
                  PVReinComm = DBI::dbDataType(conn, numeric()),
                  PVReinBen = DBI::dbDataType(conn, numeric()),
                  PVReinPremRfnd = DBI::dbDataType(conn, numeric()),
                  PVReinCommRfnd = DBI::dbDataType(conn, numeric())
               ),
               primaryKey = c("JobId", "CovId")
            )
            CreateIndex(conn, tableName = "ValuSumm", indexCol = "PlanId")
         }
      )
   }
)

setMethod(
   f = "CreateTable.Cf",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbWithTransaction(
         conn,
         {
            CreateTable(
               conn,
               tableName = "Cf",
               colSpec <- c(
                  JobId = DBI::dbDataType(conn, character()),
                  CovId = DBI::dbDataType(conn, character()),
                  Timeline = DBI::dbDataType(conn, character()),
                  Prem = DBI::dbDataType(conn, numeric()),
                  PremTax = DBI::dbDataType(conn, numeric()),
                  Comm = DBI::dbDataType(conn, numeric()),
                  CommOvrd = DBI::dbDataType(conn, numeric()),
                  BenDth = DBI::dbDataType(conn, numeric()),
                  BenMat = DBI::dbDataType(conn, numeric()),
                  BenSur = DBI::dbDataType(conn, numeric()),
                  BenAnu = DBI::dbDataType(conn, numeric()),
                  BenDthPUA = DBI::dbDataType(conn, numeric()),
                  BenMatPUA = DBI::dbDataType(conn, numeric()),
                  BenSurPUA = DBI::dbDataType(conn, numeric()),
                  ExpnsAcq = DBI::dbDataType(conn, numeric()),
                  ExpnsMnt = DBI::dbDataType(conn, numeric()),
                  ReinPrem = DBI::dbDataType(conn, numeric()),
                  ReinComm = DBI::dbDataType(conn, numeric()),
                  ReinBen = DBI::dbDataType(conn, numeric()),
                  ReinPremRfnd = DBI::dbDataType(conn, numeric()),
                  ReinCommRfnd = DBI::dbDataType(conn, numeric()),
                  TotalGross = DBI::dbDataType(conn, numeric()),
                  TotalRein = DBI::dbDataType(conn, numeric()),
                  TotalNet = DBI::dbDataType(conn, numeric())
               ),
               primaryKey = c("JobId", "CovId", "Timeline")
            )
         }
      )
   }
)

setMethod(
   f = "WriteTable.ValuSumm",
   signature = c("SQLiteConnection", "ANY"),
   definition = function(conn, data, append = TRUE, colExpandable = FALSE) {
      names(data) <- gsub("\\.", "", names(data))
      WriteTable(conn, "ValuSumm", data, append, colExpandable)
   }
)

setMethod(
   f = "WriteTable.Cf",
   signature = c("SQLiteConnection", "ANY"),
   definition = function(conn, data, append = TRUE, colExpandable = FALSE) {
      names(data) <- gsub("\\.", "", names(data))
      WriteTable(conn, "Cf", data, append, colExpandable)
   }
)

setMethod(
   f = "DeleteRows",
   signature = c("SQLiteConnection", "character", "character"),
   definition = function(conn, tableName, where = character(0L)) {
      sql <- paste("DELETE FROM", tableName)
      if (length(where) > 0) {
         sql <- paste(sql, "WHERE", where, ";")
      }
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "CompactDb",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbExecute(conn, "VACUUM")
   }
)

setMethod(
   f = "CreateTable.Pfad",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbWithTransaction(
         conn,
         {
            CreateTable(
               conn,
               tableName = "Pfad",
               colSpec <- c(
                  JobId = DBI::dbDataType(conn, character()),
                  CovId = DBI::dbDataType(conn, character()),
                  PlanId = DBI::dbDataType(conn, character()),
                  NetRes = DBI::dbDataType(conn, numeric()),
                  MortPfad = DBI::dbDataType(conn, numeric()),
                  LapsePfad = DBI::dbDataType(conn, numeric()),
                  IntrPfad = DBI::dbDataType(conn, numeric()),
                  ExpnsPfad = DBI::dbDataType(conn, numeric()),
                  PremPfad = DBI::dbDataType(conn, numeric())
               ),
               primaryKey = c("JobId", "CovId")
            )
            CreateIndex(conn, tableName = "Pfad", indexCol = "PlanId")
         }
      )
   }
)

setMethod(
   f = "WriteTable.Pfad",
   signature = c("SQLiteConnection", "ANY"),
   definition = function(conn, data, append = TRUE, colExpandable = FALSE) {
      names(data) <- gsub("\\.", "", names(data))
      WriteTable(conn, "Pfad", data, append, colExpandable)
   }
)

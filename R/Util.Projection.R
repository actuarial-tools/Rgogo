FillProjectionFromDataFrameByRow <- function(r, df, sCol){
   eCol <- GetLastColumnWithValue(df[r,], sCol)
   if (is.null(eCol) || (eCol < 6)) {
      return(NULL)
   }
   else{
      v <- as.numeric(as.vector(t(df[r,sCol:eCol])))
      names(v) <- names(df)[sCol:eCol]
      return(v)
   }
}

ReadProjectionFromDb <- function(dataSrc, datasetID, runID, covID){
   dbConn <- GetConnection(dataSrc)
   if(!IsEmptyString(covID)) selectScope = paste0("CovID = '", covID, "'")
   #Nicole 2018-08-13 mysql
   #strSQL <- paste0("SELECT * FROM [Projection] WHERE DatasetID='", datasetID, "' AND RunID='", runID, "' AND CovID='", covID, "'")
   strSQL <- paste0("SELECT * FROM Projection WHERE DatasetID='", datasetID, "' AND RunID='", runID, "' AND CovID='", covID, "'")
   strSQL <- paste(strSQL," AND SeriesType NOT LIKE 'ERROR!%'")
   dfProjection <- RODBC::sqlQuery(dbConn, strSQL, stringsAsFactors = FALSE, as.is = TRUE)
   RODBC::odbcClose(dbConn)
   sCol <- which(names(dfProjection)=="Y001")
   listProjection <- lapply(as.list(1:dim(dfProjection)[1]), function(r){return(FillProjectionFromDataFrameByRow(r, df = dfProjection, sCol))})
   listProjection <- setNames(listProjection,t(dfProjection["SeriesType"]))
   return(listProjection)
}

ReadSeriatimSummaryFromDb <- function(dataSrc, datasetID, runID, covID){
   dbConn <- GetConnection(dataSrc)
   #Nicole 2018-08-13 mysql
   #strSQL <- paste0("SELECT * FROM [SeriatimSummary] WHERE DatasetID='", datasetID, "' AND RunID = '", runID, "' AND CovID = '", covID, "'")
   strSQL <- paste0("SELECT * FROM SeriatimSummary WHERE DatasetID='", datasetID, "' AND RunID = '", runID, "' AND CovID = '", covID, "'")
   dfSeriatimSummary <- RODBC::sqlQuery(dbConn, strSQL, stringsAsFactors = FALSE, as.is = TRUE)
   RODBC::odbcClose(dbConn)
   if(dim(dfSeriatimSummary)[1] > 0){
      return(dfSeriatimSummary)
   }
   else
      return(NULL)
}

ReadCashflowFromDb <- function(dataSrc, datasetID, runID = NULL, covID = NULL){
   dbConn <- GetConnection(dataSrc)
   #if(!IsEmptyString(covID)) selectScope = paste0("CovID = '", covID, "'")
   selectScope = paste0("RunID = '", runID, "' AND CovID = '", covID, "'")
   #Nicole 2018-08-13 mysql
   #strSQL <- paste0("SELECT * FROM [Cashflow] WHERE DatasetID='", datasetID, "'")
   strSQL <- paste0("SELECT * FROM Cashflow WHERE DatasetID='", datasetID, "'")
   if (!IsEmptyString(selectScope)) {
      strSQL <- paste(strSQL," AND ", selectScope)
   }
   df <- RODBC::sqlQuery(dbConn, strSQL, stringsAsFactors = FALSE, as.is = TRUE)
   RODBC::odbcClose(dbConn)
   sCol <- which(names(df)=="Y001")
   listCashflow <- lapply(as.list(1:dim(df)[1]), function(r){return(FillCashflowFromDataFrameByRow(r, df = df, sCol))})
   listCashflow <- setNames(listCashflow,t(df["SeriesType"]))
   return(listCashflow)
}

FillCashflowFromDataFrameByRow <- FillProjectionFromDataFrameByRow

GetLastColumnWithValue <- function(ls,sCol){
   eCol <- NULL
   vals <- which(as.numeric(ls[sCol:dim(ls)[2]])!=0)
   if (length(vals) > 0) {
      eCol <- max(vals) + sCol -1
   }
   return(eCol)
}


PlotProjection <- function(projection, seriesType=NULL, path=NULL){
   if(is.null(seriesType)){
      seriesType <- names(projection)
   }
   pl <- lapply(seriesType,function(r){return(PlotBySeriesType(r, list = projection,"Projection"))})
   pl <- Filter(Negate(is.null), pl)
   if(length(pl[sapply(pl, is.null)])==0){
      ml <- marrangeGrob(pl, nrow=1, ncol=1)
      ml
   }else{
      stop("SeriesType Error.")
   }
}


PlotBySeriesType <- function(seriesType,list,plotType){
   v <- unlist(list[seriesType])
   if(!is.null(v)){
      df <- data.frame(Year=gsub(paste0(seriesType,"."),"",names(v)),
                       Projection=v)

      g <- ggplot(data=df, aes(x=Year, y=Projection)) +
         geom_bar(stat="identity",fill="steelblue") +
         #ggtitle(paste0("Cashflow for ",seriesType)) +
         ggtitle(paste0(plotType ," for ",seriesType)) +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
         theme(plot.title = element_text(hjust = 0.5))
      return(g)
   }else{
      return(NULL)
   }
}

PlotCashflow <- function(cashflow,  seriesType=NULL, path=NULL){
   if(is.null(seriesType)){
      seriesType <- names(cashflow)
   }
   pl <- lapply(seriesType,function(r){return(PlotBySeriesType(r, list = cashflow, "Cashflow"))})
   pl <- Filter(Negate(is.null), pl)
   if(length(pl[sapply(pl, is.null)])==0){
      ml <- marrangeGrob(pl, nrow=1, ncol=1)
      if(!is.null(path)){
         ggsave(path, ml,device = "pdf")
      }
      ml
   }else{
      stop("SeriesType Error.")
   }
}




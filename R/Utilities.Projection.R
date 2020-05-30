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
#   if(!IsEmptyString(covID)) selectScope = paste0("CovID = '", covID, "'")
   strSQL <- paste0("SELECT * FROM [Projection] WHERE DatasetID='", datasetID, "' AND RunID='", runID, "' AND CovID='", covID, "'")
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
   strSQL <- paste0("SELECT * FROM [SeriatimSummary] WHERE DatasetID='", datasetID, "' AND RunID = '", runID, "' AND CovID = '", covID, "'")
   dfSeriatimSummary <- RODBC::sqlQuery(dbConn, strSQL, stringsAsFactors = FALSE, as.is = TRUE)
   RODBC::odbcClose(dbConn)
   if(dim(dfSeriatimSummary)[1] > 0){
      return(dfSeriatimSummary)
   }
   else
      return(NULL)
}

#ReadCashflowFromDb <- function(dataSrc, datasetID, runID = NULL, covID = NULL){
#   dbConn <- GetConnection(dataSrc)
#   #if(!IsEmptyString(covID)) selectScope = paste0("CovID = '", covID, "'")
#   selectScope = paste0("RunID = '", runID, "' AND CovID = '", covID, "'")
#   strSQL <- paste0("SELECT * FROM [Cashflow] WHERE DatasetID='", datasetID, "'")
#   if (!IsEmptyString(selectScope)) {
#      strSQL <- paste(strSQL," AND ", selectScope)
#   }
#   df <- sqlQuery(dbConn, strSQL, stringsAsFactors = FALSE, as.is = TRUE)
#   odbcClose(dbConn)
#   sCol <- which(names(df)=="Y001")
#   listCashflow <- lapply(as.list(1:dim(df)[1]), function(r){return(FillCashflowFromDataFrameByRow(r, df = df, sCol))})
#   listCashflow <- setNames(listCashflow,t(df["SeriesType"]))
#   return(listCashflow)
#}

ReadCashflowFromDb <- function(dataSrc, datasetID, runID = NULL, selectScope = NULL){
   dbConn <- GetConnection(dataSrc)
   #if(!IsEmptyString(covID)) selectScope = paste0("CovID = '", covID, "'")
   #selectScope = paste0("RunID = '", runID, "' AND CovID = '", covID, "'")
   #strSQL <- paste0("SELECT * FROM [Cashflow] WHERE DatasetID='", datasetID, "'")
   #if (!IsEmptyString(selectScope)) {
   #   strSQL <- paste(strSQL," AND ", selectScope)
   #}
   strSQL <- "SELECT SeriesType,SUM(Y000) Y000, "
   strSQL <- paste0(strSQL," SUM(Y001) Y001, SUM(Y002) Y002,SUM(Y003) Y003, SUM(Y004) Y004, SUM(Y005) Y005,")
   strSQL <- paste0(strSQL," SUM(Y006) Y006, SUM(Y007) Y007,SUM(Y008) Y008, SUM(Y009) Y009, SUM(Y010) Y010,")
   strSQL <- paste0(strSQL," SUM(Y011) Y011, SUM(Y012) Y012,SUM(Y013) Y013, SUM(Y014) Y014, SUM(Y015) Y015,")
   strSQL <- paste0(strSQL," SUM(Y016) Y016, SUM(Y017) Y017,SUM(Y018) Y018, SUM(Y019) Y019, SUM(Y020) Y020,")
   strSQL <- paste0(strSQL," SUM(Y021) Y021, SUM(Y022) Y022,SUM(Y023) Y023, SUM(Y024) Y024, SUM(Y025) Y025,")
   strSQL <- paste0(strSQL," SUM(Y026) Y026, SUM(Y027) Y027,SUM(Y028) Y028, SUM(Y029) Y029, SUM(Y030) Y030,")
   strSQL <- paste0(strSQL," SUM(Y031) Y031, SUM(Y032) Y032,SUM(Y033) Y033, SUM(Y034) Y034, SUM(Y035) Y035,")
   strSQL <- paste0(strSQL," SUM(Y036) Y036, SUM(Y037) Y037,SUM(Y038) Y038, SUM(Y039) Y039, SUM(Y040) Y040,")
   strSQL <- paste0(strSQL," SUM(Y041) Y041, SUM(Y042) Y042,SUM(Y043) Y043, SUM(Y044) Y044, SUM(Y045) Y045,")
   strSQL <- paste0(strSQL," SUM(Y046) Y046, SUM(Y047) Y047,SUM(Y048) Y048, SUM(Y049) Y049, SUM(Y050) Y050,")
   strSQL <- paste0(strSQL," SUM(Y051) Y051, SUM(Y052) Y052,SUM(Y053) Y053, SUM(Y054) Y054, SUM(Y055) Y055,")
   strSQL <- paste0(strSQL," SUM(Y056) Y056, SUM(Y057) Y057,SUM(Y058) Y058, SUM(Y059) Y059, SUM(Y060) Y060,")
   strSQL <- paste0(strSQL," SUM(Y061) Y061, SUM(Y062) Y062,SUM(Y063) Y063, SUM(Y064) Y064, SUM(Y065) Y065,")
   strSQL <- paste0(strSQL," SUM(Y066) Y066, SUM(Y067) Y067,SUM(Y068) Y068, SUM(Y069) Y069, SUM(Y070) Y070,")
   strSQL <- paste0(strSQL," SUM(Y071) Y071, SUM(Y072) Y072,SUM(Y073) Y073, SUM(Y074) Y074, SUM(Y075) Y075,")
   strSQL <- paste0(strSQL," SUM(Y076) Y076, SUM(Y077) Y077,SUM(Y078) Y078, SUM(Y079) Y079, SUM(Y080) Y080,")
   strSQL <- paste0(strSQL," SUM(Y081) Y081, SUM(Y082) Y082,SUM(Y083) Y083, SUM(Y084) Y084, SUM(Y085) Y085,")
   strSQL <- paste0(strSQL," SUM(Y086) Y086, SUM(Y087) Y087,SUM(Y088) Y088, SUM(Y089) Y089, SUM(Y090) Y090,")
   strSQL <- paste0(strSQL," SUM(Y091) Y091, SUM(Y092) Y092,SUM(Y093) Y093, SUM(Y094) Y094, SUM(Y095) Y095,")
   strSQL <- paste0(strSQL," SUM(Y096) Y096, SUM(Y097) Y097,SUM(Y098) Y098, SUM(Y099) Y099, SUM(Y100) Y100,")
   strSQL <- paste0(strSQL," SUM(Y101) Y101, SUM(Y102) Y102,SUM(Y103) Y103, SUM(Y104) Y104, SUM(Y105) Y105,")
   strSQL <- paste0(strSQL," SUM(Y106) Y106, SUM(Y107) Y107,SUM(Y108) Y108, SUM(Y109) Y109, SUM(Y110) Y110,")
   strSQL <- paste0(strSQL," SUM(Y111) Y111, SUM(Y112) Y112,SUM(Y113) Y113, SUM(Y114) Y114, SUM(Y115) Y115,")
   strSQL <- paste0(strSQL," SUM(Y116) Y116, SUM(Y117) Y117,SUM(Y118) Y118, SUM(Y119) Y119, SUM(Y120) Y120 ")
   strSQL <- paste0(strSQL," FROM [Cashflow] JOIN [CovData] on [Cashflow].DatasetID=[CovData].DatasetID and [Cashflow].CovID=[CovData].CovID")
   strSQL <- paste0(strSQL," WHERE [Cashflow].DatasetID='", datasetID, "' AND [Cashflow].RunID='", runID, "' ")
   strSQL <- paste0(strSQL," AND [Cashflow].SeriesType NOT LIKE 'ERROR!%'")
   if (IsEmptyString(selectScope) == FALSE) {
      strSQL <- paste0(strSQL," AND ", selectScope)
   }
   strSQL <- paste0(strSQL," GROUP BY [Cashflow].SeriesType")
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
   return(pl)
   #if(length(pl[sapply(pl, is.null)])==0){
   #   ml <- marrangeGrob(pl, nrow=1, ncol=1)
   #   ml
   #}else{
   #   stop("SeriesType Error.")
   #}
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
   return(pl)
   #if(length(pl[sapply(pl, is.null)])==0){
   #   ml <- marrangeGrob(pl, nrow=1, ncol=1)
   #   if(!is.null(path)){
   #      ggsave(path, ml,device = "pdf")
   #   }
   #   ml
   #}else{
   #   stop("SeriesType Error.")
   #}
}



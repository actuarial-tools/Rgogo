# setClass(Class = "IProject", slots = list(ProjectName = "character", DataSrc = "IDataSource", DbName = "character", DbFilePath = "character"))
setClass(Class = "IProject", slots = list(ProjectName = "character"))

# setGeneric(name = "GetBasicInfo", def = function(prj = "IProject"){standardGeneric("GetBasicInfo")})
setGeneric(name = "GetSettings", def = function(prj = "IProject"){standardGeneric("GetSettings")})
setGeneric(name = "GetDataSource", def = function(prj = "IProject"){standardGeneric("GetDataSource")})
setGeneric(name = "Create", def = function(prj = "IProject"){standardGeneric("Create")})
setGeneric(name = "Delete", def = function(prj = "IProject"){standardGeneric("Delete")})

# setGeneric(name = "MaintainPrjDb", def = function(prj = "IProject"){standardGeneric("MaintainPrjDb")})
# setGeneric(name = "DeletePrjDb", def = function(prj = "IProject"){standardGeneric("DeletePrjDb")})


setMethod(
   f = "GetSettings",
   signature = "IProject",
   definition = function(prj){
      if(prj@ProjectName %in% rownames(installed.packages())){
         return(packageDescription(pkg = prj@ProjectName))
      } else {
         stop(paste0("Project (package) \"", prj@ProjectName, "\" is not installed."))
      }
   }
)


setMethod(
   f = "GetDataSource",
   signature = "IProject",
   definition = function(prj){
      settings <- GetSettings(prj)
      if(!is.null(settings$RGG_DataSourceClass)){
         if(!is.null(settings$RGG_ConnStr)){
            eval(expr = parse(text = paste0("dataSrc <- ", settings$RGG_DataSourceClass, "(settings$RGG_ConnStr)")))
            return(dataSrc)
         } else {
            stop(paste0("'RGG_ConnStr' is not specified in ", prj@ProjectName, " DESCRIPTION file."))
         }
      } else {
         stop(paste0("'RGG_DataSourceClass' is not specified in ", prj@ProjectName, " DESCRIPTION file."))
      }
   }
)



setMethod(
   f = "Delete",
   signature = "IProject",
   definition = function(prj){
      dataSrc <- GetDataSource(prj)
      if(DbExists(dataSrc)) DeleteDb(dataSrc) else message(paste0("Nothing to delete: ", prj@ProjectName))
   }
)



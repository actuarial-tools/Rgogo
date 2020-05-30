setClass(Class = "IRein", contains = c("IObject", "VIRTUAL"))


# Wrapper method of GetId
setMethod(
   f = "GetTreatyId",
   signature = "IRein",
   definition = function(object) {
      return(object@Id)
   }
)


# Wrapper method of SetId<-
setMethod(
   f = "SetTreatyId<-",
   signature = "IRein",
   definition = function(object, value) {
      object@Id <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "ProjPrem",
   signature = "IRein",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjPrem' must be implemented by a class inheriting 'IRein'" )
   }
)


setMethod(
   f = "ProjComm",
   signature = "IRein",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjComm' must be implemented by a class inheriting 'IRein'" )
   }
)


setMethod(
   f = "ProjReinNaar",
   signature = "IRein",
   definition = function(object, cov, resultContainer) {
      stop("Method 'ProjDthBen' must be implemented by a class inheriting 'IRein'" )
   }
)


setMethod(
   f = "Project",
   signature = "IRein",
   definition = function(object, cov, resultContainer) {
      stop("Method 'Project' must be implemented by a class inheriting 'IRein'" )
   }
)


setMethod(
   f = "SaveAsRda",
   signature = "IRein",
   definition = function(object, overwrite = FALSE) {
      stopifnot(HasValue(treatyId <- GetId(object)))
      rdaName <- paste0(ifelse(startsWith(treatyId, "Rein."), "", "Rein."), treatyId)
      eval(parse(text = paste(rdaName, "<- object")))
      eval(parse(text = paste("usethis::use_data(", rdaName, ", overwrite = ", overwrite, ")")))
   }
)

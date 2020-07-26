setClass(Class = "IRein", contains = c("IObject", "VIRTUAL"))

setValidity(
   Class = "IRein",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "Rein.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'Rein.'"
         }
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetReinId",
   signature = "IRein",
   definition = function(object) {
      return(object@Id)
   }
)

setMethod(
   f = "SetReinId<-",
   signature = "IRein",
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "Rein.")) {
         value <- paste0("Rein.", value)
      }
      SetId(object) <- value
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


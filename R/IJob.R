#' @include IModel.R
NULL

setClassUnion(name = "function_or_character", members = c("function", "character"))
setClassUnion(name = "logical_or_character", members = c("logical", "character"))

setClass(
   Class = "IJob",
   contains = c("VIRTUAL", "IObject"),
   slots = c(
      InpVars = "list",
      Dispatcher = "function_or_character",
      ExportRda = "logical_or_character",
      ExportExcel = "logical_or_character",
      DbDriver = "ANY",
      DbConnArgs = "character"
   )
)

setValidity(
   Class = "IJob",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "Job.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'Job.'"
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
   f = "GetJobId",
   signature = "IJob",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetJobId<-",
   signature = c("IJob", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "Job.")) {
         value <- paste0("Job.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetInpVars",
   signature = "IJob",
   definition = function(object) {
      return(object@InpVars)
   }
)

setMethod(
   f = "SetInpVars<-",
   signature = "IJob",
   definition = function(object, value) {
      object@InpVars <- as.list(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetDispatcher<-",
   signature = "IJob",
   definition = function(object, func) {
      object@Dispatcher <- func
      return(object)
   }
)

setMethod(
   f = "ExportToRda<-",
   signature = "IJob",
   definition = function(object, value) {
      object@ExportRda <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ExportToRda",
   signature = "IJob",
   definition = function(object) {
      if (object@ExportRda != FALSE) {
         warning("Method 'ExportToRda' is not implemented.")
      }
      invisible(NULL)
   }
)

setMethod(
   f = "ExportToExcel<-",
   signature = "IJob",
   definition = function(object, value) {
      object@ExportExcel <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ExportToExcel",
   signature = "IJob",
   definition = function(object) {
      if (object@ExportExcel!= FALSE) {
         warning("Method 'ExportToExcel' is not implemented.")
      }
      invisible(NULL)
   }
)

setMethod(
   f = "Dispatch",
   signature = "IJob",
   definition = function(object, inpVar) {
      return(do.call(object@Dispatcher, list(inpVar)))
   }
)

setMethod(
   f = "Initialize",
   signature = "IJob",
   definition = function(object) {
      return(object)
   }
)

setMethod(
   f = "Run",
   signature = "IJob",
   definition = function(object) {
      msg <- New.SysMessage()
      AddMessage(msg) <- paste0("Job starts at: ", jobStartTime <- Sys.time())
      object <- Initialize(object)
      result <- lapply(
         GetInpVars(object),
         function(inp, job) {
            m <- Dispatch(job, inp)
            if (is.null(m)) {return(NULL)}
            rslt <- Run(m, inp, list())
            return(rslt)
         },
         object
      )
      result <- Finalize(object, result)
      jobEndTime <- Sys.time()
      AddMessage(msg) <- paste0("Job ends at: ", jobEndTime <- Sys.time())
      AddMessage(msg) <- paste0("Time lapsed: ", round(as.double(difftime(jobEndTime, jobStartTime, units = "min")), digits = 3), " min")
      cat(GetMessage(msg), sep = "\n")
      invisible(result)
   }
)

setMethod(
   f = "Finalize",
   signature = "IJob",
   definition = function(object, jobResult) {
      # The method must return a list containing the final results.
      return(jobResult)
   }
)

setMethod(
   f = "RunParallel",
   signature = "IJob",
   definition = function(object, reqPkgs = (.packages()), cores = NA_integer_) {
      msg <- New.SysMessage()
      AddMessage(msg) <- paste0("Job starts at: ", jobStartTime <- Sys.time())
      object <- Initialize(object)
      if(!"Rgogo" %in% reqPkgs) reqPkgs <- c("Rgogo", reqPkgs)
      totalCores <- parallel::detectCores()
      # In R 4.0.0, there appear to be a bug when using makeCluster in MacOS.  A way to work around is to add argument setup_strategy = "sequential".
      # This issue does not exist when running R 4.0.0 in Windows 10.
      if(is.na(cores)){
         cl <- parallel::makeCluster(max(totalCores - 1,1), setup_strategy = "sequential")
      } else {
         cl <- parallel::makeCluster(max(min(cores, totalCores),1), setup_strategy = "sequential")
      }
      for(pkg in reqPkgs){
         eval(parse(text = paste0("parallel::clusterEvalQ(cl, expr = library(", pkg, "))")))
      }
      parallel::clusterExport(cl, "object", envir = environment())
      result <- parallel::parLapply(
         cl,
         X = GetInpVars(object),
         fun = function(inp, job) {
            m <- Dispatch(job, inp)
            if (is.null(m)) {return(NULL)}
            rslt <- Run(m, inp, list())
            return(rslt)
         },
         object
      )
      parallel::stopCluster(cl)
      result <- Finalize(object, result)
      jobEndTime <- Sys.time()
      AddMessage(msg) <- paste0("Job ends at: ", jobEndTime <- Sys.time())
      AddMessage(msg) <- paste0("Time lapsed: ", round(as.double(difftime(jobEndTime, jobStartTime, units = "min")), digits = 3), " min")
      cat(GetMessage(msg), sep = "\n")
      invisible(result)
   }
)

setMethod(
   f = "ConnectDb",
   signature = "IJob",
   definition = function(object) {
      if (HasValue(object@DbDriver)) {
         return(ConnectDb(object@DbDriver, object@DbConnArgs))
      } else {
         return(NULL)
      }
   }
)

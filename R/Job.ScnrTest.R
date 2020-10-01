setClass(
   Class = "Job.ScnrTest",
   contains = "IJob"
)

Job.ScnrTest <- function(inpVars, dispatcher, id = character(0L), descrip = character(0L)) {
   job <- new(
      Class = "Job.ScnrTest",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setMethod(
   f = "Finalize",
   signature = "Job.ScnrTest",
   definition = function(object, result, digits = 0) {
      outputDir <- paste0("output/", GetId(object))
      dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]][[1]]"), collapse = ","), ")")
      df <- eval(expr = parse(text = s))
      df <- cbind(JobId = GetId(object), df)
      return(list(ScnrResult = df))
   }
)



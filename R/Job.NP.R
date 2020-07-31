setClass(Class = "Job.NP", contains = "IJob")

Job.NP <- function(inpVars = list(),
                   dispatcher = character(0L),
                   exportToRda = FALSE,
                   exportToExcel = FALSE,
                   id = character(0L),
                   descrip = character(0L)) {
   job <- new(
      Class = "Job.NP",
      InpVars = as.list(inpVars),
      Dispatcher = dispatcher,
      ExportRda = FALSE,
      ExportExcel = exportToExcel,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setMethod(
   f = "Finalize",
   signature = "Job.NP",
   definition = function(object, jobResult) {
      npResult <- lapply(
         X = jobResult,
         FUN = function(rslt, job) {
            return(rslt$NP)
         },
         object
      )
      s <- paste0("rbind(", paste0(paste0("npResult[[", 1:length(npResult), "]]"), collapse = ","), ")")
      npResult <- eval(expr = parse(text = s))
      result <- list(
         NP = npResult
      )
      return(result)
   }
)







setClass(
   Class = "Job.PfadAnlys",
   contains = "IJob"
)

Job.PfadAnlys <- function(inpVars, dispatcher, dbDrvr, dbConnArgs, id, descrip = character(0L)) {
   job <- new(
      Class = "Job.PfadAnlys",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setMethod(
   f = "Initialize",
   signature = "Job.PfadAnlys",
   definition = function(object) {
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         DeleteRows(conn, "Pfad", paste0("JobId = '", GetId(object), "'"))
         DisconnectDb(conn)
      }
      return(object)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.PfadAnlys",
   definition = function(object, result, digits = 0) {
      pfad <- cbind(JobId = GetId(object), To.data.frame(result, "Pfad"))
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.Pfad(conn, pfad)
         CompactDb(conn)
         DisconnectDb(conn)
      }
      return(list(Pfad = pfad))
   }
)



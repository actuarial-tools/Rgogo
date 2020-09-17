setClass(
   Class = "Job.PfadAnlys",
   contains = "IJob"
)

setMethod(
   f = "Initialize",
   signature = "Job.PPM",
   definition = function(object) {
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         if (object@DbAppend == FALSE) {
            DeleteRows(conn, "Pfad", paste0("JobId = '", GetId(object), "'"))
         }
         DisconnectDb(conn)
      }
      return(object)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.PfadAnlys",
   definition = function(object, result, digits = 0) {
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$Pfad"), collapse = ","), ")")
      eval(expr = parse(text = paste0("pfad <- ", s)))
      pfad <- cbind(JobId = GetId(object), pfad)
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.Pfad(conn, pfad)
         CompactDb(conn)
         DisconnectDb(conn)
      }
      invisible(list(Pfad = pfad))
   }
)



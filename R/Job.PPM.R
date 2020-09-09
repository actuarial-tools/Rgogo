setClass(
   Class = "Job.PPM",
   contains = "IJob",
   slots = c(Conn = "DBIConnection")
)

Job.PPM <- function(inpVars, dispatcher, conn, id, descrip = character(0L)) {
   job <- new(
      Class = "Job.PPM",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      Conn = conn,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setMethod(
   f = "GetConn",
   signature = "Job.PPM",
   definition = function(object) {
      return(object@Conn)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.PPM",
   definition = function(object, result, digits = 0) {

      # Valuation summary
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$ValuSumm"), collapse = ","), ")")
      eval(expr = parse(text = paste0("valuSumm <- ", s)))
      valuSumm <- cbind(JobId = GetId(object), valuSumm)

      # Cash value
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$Cf"), collapse = ","), ")")
      eval(expr = parse(text = paste0("cf <- ", s)))
      cf <- cbind(JobId = GetId(object), cf)

      # PV
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$PV"), collapse = ","), ")")
      eval(expr = parse(text = paste0("pv <- ", s)))
      pv <- cbind(JobId = GetId(job), pv)

      jobResut <- list(
         ValuSumm = valuSumm,
         Cf = cf,
         PV = pv
      )


      #    rdaFileName <- "Cf"
      #    eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
      #    eval(expr = parse(text = paste0("save(", rdaFileName, ", file = paste0(outputDir, '/', rdaFileName, '.rda'))")))



      invisible(jobResut)




      # outputDir <- paste0("output/", GetId(object))
      # if (object@SaveValuSumm) {
      #    s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$ValuSumm"), collapse = ","), ")")
      #    rdaFileName <- "ValuSumm"
      #    eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
      #    eval(expr = parse(text = paste0("save(", rdaFileName, ", file = paste0(outputDir, '/', rdaFileName, '.rda'))")))
      # }
      # if (object@SaveCf) {
      #    s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$Cf"), collapse = ","), ")")
      #    rdaFileName <- "Cf"
      #    eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
      #    eval(expr = parse(text = paste0("save(", rdaFileName, ", file = paste0(outputDir, '/', rdaFileName, '.rda'))")))
      # }
      # return(result)
   }
)

ExportToExcel.Job.PPM <- function(result, dir, annualized = TRUE, digits = 0, overwrite = FALSE) {
   lst <- lapply(result,
                 function(rslt, d, anlz, dgt, ow) {ExportToExcel.Model.PPM(rslt, d, anlz, dgt, ow)},
                 dir, annualized, digits, overwrite)
   n <- sum(unlist(lst))
   return(cat("Number of Excel files created:", n))
}





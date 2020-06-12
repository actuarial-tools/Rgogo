setClass(
   Class = "Job.PPM",
   contains = "IJob"
)

setMethod(
   f = "Finalize",
   signature = "Job.PPM",
   definition = function(object, result, digits = 0) {
      outputDir <- paste0("output/", GetId(object))
      if (object@SaveValuSumm) {
         s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$ValuSumm"), collapse = ","), ")")
         rdaFileName <- "ValuSumm"
         eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
         eval(expr = parse(text = paste0("save(", rdaFileName, ", file = paste0(outputDir, '/', rdaFileName, '.rda'))")))
      }
      if (object@SaveCf) {
         s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$Cf"), collapse = ","), ")")
         rdaFileName <- "Cf"
         eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
         eval(expr = parse(text = paste0("save(", rdaFileName, ", file = paste0(outputDir, '/', rdaFileName, '.rda'))")))
      }
      return(result)
   }
)

ExportToExcel.Job.PPM <- function(result, dir, annualized = TRUE, digits = 0, overwrite = FALSE) {
   lst <- lapply(result,
                 function(rslt, d, anlz, dgt, ow) {ExportToExcel.Model.PPM(rslt, d, anlz, dgt, ow)},
                 dir, annualized, digits, overwrite)
   n <- sum(unlist(lst))
   return(cat("Number of Excel files created:", n))
}





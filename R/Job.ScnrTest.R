#' @include IJob.R
NULL


setClass(
   Class = "Job.ScnrSet",
   contains = "IJob",
)


setMethod(
   f = "Finalize",
   signature = "Job.ScnrSet",
   definition = function(object, result, digits = 0) {
      outputDir <- paste0("output/", GetId(object))
      dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]"), collapse = ","), ")")
      df <- eval(expr = parse(text = s))
      return(df)
   }
)



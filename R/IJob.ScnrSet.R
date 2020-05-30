#' @include IJob.R
NULL


setClass(
   Class = "IJob.ScnrSet",
   contains = c("IJob", "VIRTUAL")
)


setMethod(
   f = "Dispatch",
   signature = "IJob.ScnrSet",
   definition = function(object, inpVar) {
      stop("Method 'Dispatch' must be implemented by a class extending 'IJob.ScnrSet' virtual class.")
   }
)


setMethod(
   f = "Initialize",
   signature = "IJob.ScnrSet",
   definition = function(object) {
      callNextMethod()
   }
)


setMethod(
   f = "Finalize",
   signature = "IJob.ScnrSet",
   definition = function(object, result, digits = 0) {
      outputDir <- paste0("output/", GetId(object))
      dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]"), collapse = ","), ")")
      df <- eval(expr = parse(text = s))
      return(df)
   }
)



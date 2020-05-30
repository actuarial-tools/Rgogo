#' @include IJob.R
NULL


setClass(
   Class = "IJob.PfadAnlys",
   contains = c("IJob", "VIRTUAL")
)


setMethod(
   f = "Dispatch",
   signature = "IJob.PfadAnlys",
   definition = function(object, inpVar) {
      stop("Method 'Dispatch' must be implemented by a class extending 'IJob.PfadAnlys' virtual class.")
   }
)


setMethod(
   f = "Initialize",
   signature = "IJob.PfadAnlys",
   definition = function(object) {
      callNextMethod()
   }
)


setMethod(
   f = "Finalize",
   signature = "IJob.PfadAnlys",
   definition = function(object, result, digits = 0) {
      outputDir <- paste0("output/", GetId(object))
      dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
      s <- paste0("rbind(", paste0(paste0("result[[", 1:length(result), "]]$PfadInfo"), collapse = ","), ")")
      rdaFileName <- "PfadInfo"
      path <- paste0(outputDir, '/', rdaFileName, '.rda')
      eval(expr = parse(text = paste0(rdaFileName, "<-", s)))
      eval(expr = parse(text = paste0("save(", rdaFileName, ", file = path)")))
      cat(path, "has been created.\n")
      return(result)
   }
)



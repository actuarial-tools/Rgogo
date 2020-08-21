setClass(
   Class = "RgogoProj",
   contains = "IObject",
   slots = c(
      Path = "character"
   )
)

RgogoProj <- function(id, path) {
   Proj <- new(Class = "RgogoProj", Id = id, Path = path)
   return(Proj)
}

setMethod(
   f = "GetPath",
   signature = "RgogoProj",
   definition = function(object) {
      return(object@Path)
   }
)

setMethod(
   f = "ReadDescripFile",
   signature = "RgogoProj",
   definition = function(object) {
      path <- file.path(GetPath(object), "DESCRIPTION")
      if (!file.exists(path)) {
         return(NULL)
      }
      f <- file(path)
      txt <- readLines(f)
      close(f)
      content <- list()
      for (s in txt) {
         sep <- regexpr(":", s)
         if (sep > 0) {
            key <- trimws(substr(s, 1, sep - 1))
            value <- trimws(substr(s, sep + 1, nchar(s)))
            if (nchar(value) > 0) {
               content <- c(content, eval(expr = parse(text = paste0("list('", key, "' = '", value, "')"))))
            } else {
               content <- c(content, eval(expr = parse(text = paste0("list('", key, "' = character(0L))"))))
            }
         } else {
            if (nchar(trimws(s)) > 0) {
               content[[length(content)]] <- c(content[[length(content)]], trimws(s))
            }
         }
      }
      if (!is.null(content$Imports)) {
         content$Imports <- gsub(",", "", content$Imports)
      }
      if (!is.null(content$Suggests)) {
         content$Suggests <- gsub(",", "", content$Suggests)
      }
      return(content)
   }
)

setMethod(
   f = "WriteDescripFile",
   signature = "RgogoProj",
   definition = function(object, descripFields) {
      f <- file(file.path(GetPath(object), "DESCRIPTION"))
      s <- character()
      for (key in names(descripFields)) {
         value <- eval(expr = parse(text = paste0("descripFields$'", key, "'")))
         if (key %in% c("Imports", "Suggests")) {
            sep = ",\n    "
         } else {
            sep = "\n    "
         }
         s <- c(s, paste0(key, ": ", paste0(value, collapse = sep)))
      }
      writeLines(s, f)
      close(f)
   }
)

setMethod(
   f = "ReadNamespaceFile",
   signature = "RgogoProj",
   definition = function(object) {
      path <- file.path(GetPath(object), "NAMESPACE")
      if (!file.exists(path)) {
         return(NULL)
      }
      f <- file(path)
      txt <- readLines(f)
      close(f)
      return(txt)
   }
)

setMethod(
   f = "WriteNamespaceFile",
   signature = "RgogoProj",
   definition = function(object, content) {
      f <- file(file.path(GetPath(object), "NAMESPACE"))
      writeLines(content, f)
      close(f)
   }
)

setMethod(
   f = "Create",
   signature = "RgogoProj",
   definition = function(object, msgPath = character(0L)) {
      projId <- Rgogo::GetId(object)
      projRoot <- GetPath(object)
      cond <- tryCatch(
         {
            # Create project root directory:
            if (dir.exists(projRoot)) {
               message("The project root directory already exists.", appendLF = FALSE)
            }
            dir.create(projRoot)
            # Create DESCRIPTION file:
            descripFields <- list(
               Package = Rgogo::GetId(object),
               Type = "Package",
               Version = "0.0.1",
               License = "To be specified",
               Title = "To be specified",
               Description = "To be specified",
               Author = "To be specified",
               Maintainer = "To be specified <someone@somedomain.com>",
               Encoding = "UTF-8",
               LazyData = "true",
               Depends = "R (>= 4.0.0)",
               Imports = "Rgogo"
            )
            WriteDescripFile(object, descripFields)
            # Create NAMESPACE file:
            content <- c(
               'exportPattern("^[[:alpha:]]+")',
               'import("Rgogo")'
            )
            WriteNamespaceFile(object, content)
            # Create sub-folders under project root.
            dir.create(file.path(projRoot, "batch"))
            dir.create(file.path(projRoot, "data"))
            dir.create(file.path(projRoot, "data-raw"))
            dir.create(file.path(projRoot, "datasets"))
            dir.create(file.path(projRoot, "R"))
            NULL
         },
         message = function(c) {c},
         warning = function(c) {c},
         error = function(c) {c}
      )
      GenJsonMsgPacket(
         cond = cond, 
         jsonPath =  msgPath, 
         successMsg = paste0("Project '", Rgogo::GetId(object), "' has been created successfully."),
         warningMsg = NA_character_, 
         errorMsg = NA_character_,
         ProjectId = Rgogo::GetId(object),
         Path = projRoot
      )
   }
)

GenJsonMsgPacket <- function(cond, jsonPath, successMsg, warningMsg = NA_character_, errorMsg = NA_character_, ...) {
   stopifnot(is.null(cond) || is(cond, "condition"))
   stopifnot(length(successMsg) == 1)
   stopifnot(length(warningMsg) == 1)
   stopifnot(length(errorMsg) == 1)
   stopifnot(length(jsonPath) <= 1)
   if (is.null(cond)) {
      status <- 0
      msg <- successMsg
   } else if (is(cond, "message")) {
      status <- 1
      msg <- cond$message
   } else if (is(cond, "warning")) {
      status <- 2
      if (is.na(warningMsg)) {
         msg <- cond$message
      } else {
         msg <- warningMsg
      }
   } else if (is(cond, "error")) {
      status <- 3
      if (is.na(errorMsg)) {
         msg <- cond$message
      } else {
         msg <- errorMsg
      }
   } else {
      stop("Fail to handle an unknown condition.")
   }
   msgPacket <- list(
      Status = status,
      Message = msg,
      Details = list(...)
   )
   if (length(jsonPath) > 0) {
      jsonlite::write_json(msgPacket, jsonPath, pretty = TRUE)
   }
}


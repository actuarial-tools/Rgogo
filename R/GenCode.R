GenCode <- function(objectId, objectType, constructor,
                    fileName = NA_character_, folder = "R", jsonMsgPath = NA_character_,
                    successMsg = NA_character_, warningMsg = NA_character_, errorMsg = NA_character_, ...) {
   objectType <- ifelse(endsWith(objectType, "."), objectType, paste0(objectType, "."))
   objectId <- ifelse(startsWith(objectId, objectType), objectId, paste0(objectType, objectId))
   fileName <- ifelse(is.na(fileName), paste0(objectId, ".R"), fileName)
   fileName <- ifelse(endsWith(fileName, ".R"), fileName, paste0(fileName, ".R"))
   folder <- path.expand(folder)
   cond <- tryCatch(
      {
         if (!dir.exists(folder)) {
            message("The folder '", folder, "' does not exist.", appendLF = FALSE)
         }
         path <- file.path(folder, fileName)
         if (file.exists(path)) {
            message("The file '", path, "' already exists.", appendLF = FALSE)
         }
         # Start generating codes
         argList <- list(...)   # These are the named arguments to be passed to constructor.
         #tab <- Editor.Tab()
         tab <- GetValue(Const.Editor.Tab)
         s <- paste0('New.', objectId, ' <- function() {')
         argList$id <- objectId
         s <- c(s, paste0(tab, .GenCode.FuncCall(funcName = constructor, argList)))
         s <- c(s, "}")
         # Write to R file
         f <- file(path)
         writeLines(s, f)
         close(f)
         if (is.na(successMsg)) {
            successMsg <- paste0("File '", path, "' is created successfully.")
         }
         AddHandler.GenCode.Success(
            successMsg, jsonMsgPath,
            ObjectId = objectId, ObjectType = objectType, FileName = fileName, Folder = folder
         )
         NULL
      },
      message = function(cond){
         AddHandler.GenCode.Message(
            cond, msg = cond$message, jsonMsgPath,
            ObjectId = objectId, ObjectType = objectType, FileName = fileName, Folder = folder
         )
         return(cond)
      },
      warning = function(cond) {
         AddHandler.GenCode.Message(
            cond, msg = ifelse(is.na(warningMsg), cond$message, warningMsg), jsonMsgPath,
            ObjectId = objectId, ObjectType = objectType, FileName = fileName, Folder = folder
         )
         unlink(path)
         return(cond)
      },
      error = function(cond) {
         AddHandler.GenCode.Message(
            cond, msg = ifelse(is.na(errorMsg), cond$message, errorMsg), jsonMsgPath,
            ObjectId = objectId, ObjectType = objectType, FileName = fileName, Folder = folder
         )
         unlink(path)
         return(cond)
      }
   )
   return(cond)
}

AddHandler.GenCode.Success <- function(msg = NA_character_, jsonPath, ...) {
   WriteJsonMsgPacket(status = 0, msg, jsonPath, ...)
}

AddHandler.GenCode.Message <- function(cond, msg = NA_character_, jsonPath, ...) {
   if (is.na(msg)) {
      msg <- cond$message
   }
   WriteJsonMsgPacket(status = 1, msg, jsonPath, ...)
}

AddHandler.GenCode.Warning <- function(cond, msg = NA_character_, jsonPath, ...) {
   if (is.na(msg)) {
      msg <- cond$message
   }
   WriteJsonMsgPacket(status = 2, msg, jsonPath, ...)
}

AddHandler.GenCode.Error <- function(cond, msg = NA_character_, jsonPath, ...) {
   if (is.na(msg)) {
      msg <- cond$message
   }
   WriteJsonMsgPacket(status = 3, msg, jsonPath, ...)
}

WriteJsonMsgPacket <- function(status, msg, jsonPath, ...) {
   msgPacket <- list(
      Status = status,
      Message = msg,
      Details = list(...)
   )
   if (!is.na(jsonPath) > 0) {
      jsonlite::write_json(msgPacket, jsonPath, pretty = TRUE)
   }
}

.GenCode.FuncCall <- function(funcName, args) {
   func <- eval(expr = parse(text = funcName))
   funcArgs <- lapply(formals(func), FUN = function(x) {deparse1(x)})
   if (length(args) > 0) {
      for (i in 1:length(args)) {
         if (names(args)[i] %in% names(funcArgs)) {
            funcArgs[[names(args)[i]]] <- deparse1(args[[i]])
         } else {
            stop("Unknown argument:", names(args)[i])
         }
      }
   }
   # tab <- Editor.Tab()
   tab <- GetValue(Const.Editor.Tab)
   s <- paste0('object <- ', funcName, '(')
   if (length(funcArgs) > 0) {
      for (i in 1:length(funcArgs)) {
         v <- eval(expr = parse(text = funcArgs[[i]]))
         s <- c(s, paste0(tab, names(funcArgs)[i], ' = ', funcArgs[[i]], ","))
      }
      s[length(s)] <- substr(s[length(s)], 1, nchar(s[length(s)]) - 1)
   } else {
      s <- paste0(s, ')')
   }
   s <- c(s, ")")
   s <- c(s, "return(object)")
   return(s)
}

GenCode.Plan <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "Plan",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.MortAssump <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "MortAssump",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.IntrAssump <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "IntrAssump",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.LapseAssump <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "LapseAssump",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.ExpnsAssump <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "ExpnsAssump",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.PremAssump <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "PremAssump",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.ArgSet <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "ArgSet",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

GenCode.Model <- function(objectId, constructor, jsonMsgPath = NA_character_, argSet, ...) {
   GenCode(
      objectId = objectId,
      objectType = "Model",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      args = argSet,
      ...
   )
}

GenCode.Rein <- function(objectId, constructor, jsonMsgPath = NA_character_, ...) {
   GenCode(
      objectId = objectId,
      objectType = "Rein",
      constructor = constructor,
      jsonMsgPath = jsonMsgPath,
      ...
   )
}

InspectFuncArgs <- function(func, jsonPath = NA_character_) {
   stopifnot(is.function(func) | is.character(func))
   if (is.function(func)) {
      argList <- formals(func)
   } else {
      argList <- formals(eval(expr = parse(text = func)))
   }
   argName <- names(argList)
   argDescrip <- Rgogo::Translate(argName, lang = "en", strCase = "title")
   defaultValue <- unlist(lapply(argList, function(x) {deparse1(x)}))
   df <- data.frame(ArgName = argName, ArgDescrip = argDescrip, DefaultValue = defaultValue, stringsAsFactors = FALSE)
   rownames(df) <- NULL
   if (is.na(jsonPath)) {
      return(df)
   } else {
      jsonlite::write_json(df, jsonPath, pretty = TRUE)
      return(path.expand(jsonPath))
   }
}



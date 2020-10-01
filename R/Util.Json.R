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

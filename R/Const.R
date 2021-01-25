setClass(
   Class = "Const",
   contains = "IObject",
   slots = c(
      Value = "ANY"
   )
)

Const <- function(value = NULL, id = character(0L), descrip = character(0L)) {
   object <- new(
      Class = "Const",
      Value = value,
      Descrip = descrip
   )
   SetId(object) <- id
   return(object)
}

setMethod(
   f = "SetConstId<-",
   signature = c("Const", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "Const.")) {
         value <- paste0("Const.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetValue",
   signature = "Const",
   definition = function(object) {
      return(object@Value)
   }
)

setClass(
   Class = "IValidator",
   contains = c("IObject", "VIRTUAL")
)


setGeneric(name = "Validate", def = function(object, value) {standardGeneric("Validate")})


setMethod(
   f = "Validate",
   signature = c("IValidator", "ANY"),
   definition = function(object, value) {
      stop("'Validate' method must be implemented in a class extending 'IValidator' virtual class.")
   }
)

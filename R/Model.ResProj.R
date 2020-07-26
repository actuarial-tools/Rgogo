setClass(
   Class = "Model.ResProj",
   contains = "IModel",
   slots = (Args = "IArgSet.ResProj")
)

Model.ResProj <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.ResProj", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "GetArgsList",
   signature = "Model.ResProj",
   definition = function(object) {
      return(GetArgsList(GetArgs(object)))
   }
)

setMethod(
   f = "GetValuDates",
   signature = "Model.ResProj",
   definition = function(object) {
      return(GetValuDates(GetArgs(object)))
   }
)

setMethod(
   f = "GetModel",
   signature = "Model.ResProj",
   definition = function(object) {
      return(GetModel(GetArgs(object)))
   }
)

setMethod(
   f = "Run",
   signature = "Model.ResProj",
   definition = function(object, var, result = list()) {
      valuModel <- GetModel(object)
      valuDates <- GetValuDates(object)
      argsList <- GetArgsList(object)[valuDates <= GetExpiryDate(var) & (valuDates + 1) >= GetIssDate(var)]
      if (length(argsList) == 0) {
         return(result)
      }
      res <- lapply(
         X = 1:length(argsList),
         FUN = function(i, argsList, model, cov) {
            SetArgs(model) <- argsList[[i]]
            r <- Run(model, cov, list())
            df <- data.frame(
               ValuDate = GetArgValue(argsList[[i]], "ValuDate"),
               Res.Gross = r$Res$Res.Gross,
               Res.Rein = r$Res$Res.Rein,
               Res.Net = r$Res$Res.Net,
               stringsAsFactors = FALSE
            )
            return(df)
         },
         argsList, valuModel, var
      )
      s <- paste0("rbind(", paste0(paste0("res[[", 1:length(res), "]]"), collapse = ","), ")")
      res <- eval(expr = parse(text = s))
      result$ResProj <- res
      return(result)
   }
)

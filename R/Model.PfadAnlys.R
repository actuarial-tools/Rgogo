setClass(Class = "Model.PfadAnlys", contains = "IModel")

Model.PfadAnlys <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.PfadAnlys", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = "Model.PfadAnlys",
   definition = function(object, var, result) {
      # PfAD analysis is to calculate reserves by removing provisions for adverse deviation in the following orders: mortality -> lapse -> interest -> expense -> premium
      model <- GetModel(GetArgs(object))
      args <- GetArgs(model)
      # Calculate base scenario reserve
      valuResult <- Run(model, var, list())
      netRes0 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
      # Step 1: calculate reserve by removing mortality pfad
      netRes1 <- netRes0
      if (all(Contains(args, c("MortAssump", "ApplyMortMargin")))) {
         if (ApplyMortMargin(args) & !is.null(GetMortAssump(args))) {
            ApplyMortMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            netRes1 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
         }
      }
      # Step 2: calculate reserve by removing lapse pfad
      netRes2 <- netRes1
      if (all(Contains(args, c("LapseAssump", "ApplyLapseMargin")))) {
         if (ApplyLapseMargin(args) & !is.null(GetLapseAssump(args))) {
            ApplyLapseMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            netRes2 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
         }
      }
      # Step 3: calculate reserve by removing interest pfad
      netRes3 <- netRes2
      if (all(Contains(args, c("IntrAssump", "ApplyIntrMargin")))) {
         if (ApplyIntrMargin(args) & !is.null(GetIntrAssump(args))) {
            ApplyIntrMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            netRes3 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
         }
      }
      # Step 4: calculate reserve by removing expense pfad
      netRes4 <- netRes3
      if (all(Contains(args, c("ExpnsAssump", "ApplyExpnsMargin")))) {
         if (ApplyExpnsMargin(args) & !is.null(GetExpnsAssump(args))) {
            ApplyExpnsMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            netRes4 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
         }
      }
      # Step 5: calculate reserve by removing premium pfad
      netRes5 <- netRes4
      if (all(Contains(args, c("PremAssump", "ApplyPremMargin")))) {
         if (ApplyPremMargin(args) & !is.null(GetPremAssump(args))) {
            ApplyPremMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            netRes5 <- ifelse(is.null(valuResult$Res$Res.Net), 0, valuResult$Res$Res.Net)
         }
      }
      result$Pfad <- list(
         CovId = GetId(var),
         PlanId = GetPlanId(var),
         NetRes = netRes0,
         MortPfad = netRes0 - netRes1,
         LapsePfad = netRes1 - netRes2,
         IntrPfad = netRes2 - netRes3,
         ExpnsPfad = netRes3 - netRes4,
         PremPfad = netRes4 - netRes5
      )
      return(result)
   }
)


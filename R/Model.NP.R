setClass(Class = "Model.NP", contains = "IModel")

Model.NP <- function(args = ArgSet.NP(), descrip = character(0L), id = character(0L)) {
   model <- new(Class = "Model.NP", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.NP", "Cov"),
   definition = function(object, var, result) {
      result$CovData <- var
      result$.ArgSet <- GetArgs(object)
      return(Run.NP(object, GetPlan(var), var, result))
   }
)

setMethod(
   # f = "Run.NP",
   f = "Run",
   signature = c("Model.NP", "IPlan", "Cov"),
   definition = function(object, var, cov, result) {
      args <- GetArgs(object)
      covYears <- as.integer(GetCovYears(var, cov))
      premYears <- as.integer(GetPremYears(var, cov))

      # Project policy values
      result <- Project(var, cov, result)
      proj <- result$Proj

      # Get mortality assumption information
      mortAssump <- GetMortAssump(args)
      if (!is.null(mortAssump)) {
         result <- GetAssump(mortAssump, cov, var, result)
         if (ApplyMortMargin(args)) {
            q <- result$q.Padd
         } else {
            q <- result$q.Expd
         }
      } else {
         q <- rep(0, length.out = covYears)
      }

      # Get interest assumption information
      intrAssump <- GetIntrAssump(args)
      if (!is.null(intrAssump)) {
         result <- GetAssump(intrAssump, result)
         if (ApplyIntrMargin(args)) {
            i <- result$i.Padd
         } else {
            i <- result$i.Expd
         }
         length(i) <- covYears
      } else {
         i <- rep(0, length.out = covYears)
      }
      p <- 1 - q
      pt <- ShiftRight(cumprod(p), positions = 1, filler = 1)
      v <- 1 / (1 + i)
      vt <- ShiftRight(cumprod(v), positions = 1, filler = 1)

      # Calculate net single premium of death benefit (assuming death benefit paid at the end of year)
      if (!is.null(proj$Ben.Dth)) {
         dthBen <- GetYearEndValue(proj$Ben.Dth)
         nspDthBen <- sum(dthBen * pt * q * vt * v)
      } else {
         dthBen <- rep(0, covYears)
         nspDthBen <- 0
      }

      # Calculate net single premium of maturity benefit (assuming maturity benefit paid at the end of year)
      if (!is.null(proj$Ben.Mat)) {
         matBen <- GetYearEndValue(proj$Ben.Mat)
         nspMatBen <- sum(matBen * pt * p * vt * v)
      } else {
         matBen <- rep(0, covYears)
         nspMatBen <- 0
      }

      # Caluclate present value of $1 amount of premium annuity
      anu <- FillTail(rep(1, premYears), filler = 0, len = covYears)
      nspAnu <- sum(anu * pt * vt)

      # Calculate net premium
      nspIns <- nspDthBen + nspMatBen
      np <- nspIns / nspAnu

      # Net premium reserve projection
      res <- rep(0, covYears + 1)
      for (t in 1:covYears) {
         if (t < covYears) {
            res[t + 1] <- ((res[t] + np) * (1 + i[t]) - q[t] * dthBen[t] - p[t] * matBen[t]) / p[t]   # This handles partial endowment if there is any.
         } else {
            res[t + 1] <- ((res[t] + np) * (1 + i[t]) - q[t] * dthBen[t]) / p[t]
         }
      }

      # Output result
      result$NP <- list(
         Ins = nspDthBen,
         Anu = nspAnu,
         NetPrem = np,
         Res = res,
         q = q,
         i = i
      )
      return(result)
   }
)


# Model for calculating nonforfeiture benefits

setClass(Class = "Model.NFB", contains = "Model.NP")

Model.NFB <- function(args = ArgSet.NFB(), descrip = character(0L), id = character(0L)) {
   model <- new(Class = "Model.NFB", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.NFB", "Cov"),
   definition = function(object, var, result) {
      result$CovData <- var
      result$.ArgSet <- GetArgs(object)
      return(Run(object, GetPlan(var), var, result))
   }
)

setMethod(
   # f = "Run.NFB",
   f = "Run",
   signature = c("Model.NFB", "IPlan", "Cov"),
   definition = function(object, var, cov, result) {
      model.np <- Model.NP(GetArgs(object))
      # result <- Run.NP(model.np, var, cov, result)
      result <- Run(model.np, var, cov, result)
      n <- as.integer(GetCovYears(var, cov))
      ins <- result$NP$Ins
      anu <- result$NP$Anu
      np <- result$NP$NetPrem
      res <- result$NP$Res
      faceAmt <- GetFaceAmt(cov)
      e1 <- GetArgValue(object, "A") * min(np, GetArgValue(object, "B") * faceAmt) + GetArgValue(object, "C") * faceAmt
      q <- result$NP$q
      i <- result$NP$i
      sc <- rep(0, n + 1)
      insx <- rep(0, n + 1)
      for (t in 1:n) {
         q1 <- q[t:n]
         i1 <- i[t:n]
         sc[t] <- e1 * CalcNSP.AnuDue(q1, i1, n - t + 1)
         insx[t] <- CalcNSP.End(q1, i1, n - t + 1)
      }
      insx[n + 1] <- 1
      cv <- res - sc
      cv <- cv * (cv > 0)
      rpu <- cv / insx
      result$NP$E1 <- e1
      result$NP$SC <- sc
      result$NP$Insx <- insx
      result$NP$CV <- cv
      result$NP$RPU <- rpu
      return(result)
   }
)


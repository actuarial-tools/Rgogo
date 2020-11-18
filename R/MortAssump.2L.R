setGeneric(name = "GetMortAssump1", def = function(object) {standardGeneric("GetMortAssump1")})
setGeneric(name = "GetMortAssump2", def = function(object) {standardGeneric("GetMortAssump2")})

# Last-survivor mortality assumption
setClass(
   Class = "MortAssump.2L",
   contains = "IMortAssump",
   slots = c(
      MortAssump1 = "character_or_IMortAssump",
      MortAssump2 = "character_or_IMortAssump"
   )
)

MortAssump.2L <- function(mortAssump1, mortAssump2, id = character(), descrip = character()) {
   # if (is.character(mortAssump1)) {
   #    mortAssump1 <- eval(expr = parse(text = mortAssump1))
   # }
   # if (is.character(mortAssump2)) {
   #    mortAssump2 <- eval(expr = parse(text = mortAssump2))
   # }
   assump <- new(
      Class = "MortAssump.2L",
      MortAssump1 = mortAssump1,
      MortAssump2 = mortAssump2,
      Descrip = descrip
   )
   SetAssumpId(assump) <- id
   return(assump)
}

setMethod(
   f = "GetMortAssump1",
   signature = "MortAssump.2L",
   definition = function(object) {
      slotValue <- object@MortAssump1
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "MortAssump."), slotValue, paste0("MortAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(NULL)
         }
      } else {
         return(slotValue)
      }
   }
)

setMethod(
   f = "GetMortAssump2",
   signature = "MortAssump.2L",
   definition = function(object) {
      slotValue <- object@MortAssump2
      if (is.character(slotValue)) {
         if (length(slotValue) > 0) {
            slotValue <- ifelse(startsWith(slotValue, "MortAssump."), slotValue, paste0("MortAssump.", slotValue))
            return(eval(expr = parse(text = slotValue)))
         } else {
            return(NULL)
         }
      } else {
         return(slotValue)
      }
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "MortAssump.2L",
   definition = function(object, cov, plan, assumpInfo) {
      # Get expected mortality assumption info of the first life.
      assumpInfo$.MortAssumpInfo1 <- GetExpdAssump(GetMortAssump1(object), cov, plan, assumpInfo)
      # Get expected mortality assumption info of the second life.
      SetIssAge(cov) <- GetIssAge2(cov)
      SetRiskClass(cov) <- GetRiskClass2(cov)
      if (HasValue(GetFaceAmt2(cov))) {
         SetFaceAmt(cov) <- GetFaceAmt2(cov)
      }
      assumpInfo$.MortAssumpInfo2 <- GetExpdAssump(GetMortAssump2(object), cov, plan, assumpInfo)
      # Get joint life probabilities
      assumpInfo$q_x.Expd <- assumpInfo$.MortAssumpInfo1$q.Expd
      assumpInfo$q_y.Expd <- assumpInfo$.MortAssumpInfo2$q.Expd
      return(assumpInfo)
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "MortAssump.2L",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      # Get padded mortality assumption info of the first life.
      assumpInfo$.MortAssumpInfo1 <- .Get_q_Margin(GetMortAssump1(object), cov, plan, assumpInfo$.MortAssumpInfo1, projStartDate)
      q.Padd<- assumpInfo$.MortAssumpInfo1$q.Expd + assumpInfo$.MortAssumpInfo1$q.Margin
      assumpInfo$.MortAssumpInfo1$q.Padd <- ifelse(q.Padd <= 1, q.Padd, 1)
      # Get padded mortality assumption info of the second life.
      SetIssAge(cov) <- GetIssAge2(cov)
      SetRiskClass(cov) <- GetRiskClass2(cov)
      if (HasValue(GetFaceAmt2(cov))) {
         SetFaceAmt(cov) <- GetFaceAmt2(cov)
      }
      assumpInfo$.MortAssumpInfo2 <- .Get_q_Margin(GetMortAssump2(object), cov, plan, assumpInfo$.MortAssumpInfo2, projStartDate)
      q.Padd<- assumpInfo$.MortAssumpInfo2$q.Expd + assumpInfo$.MortAssumpInfo2$q.Margin
      assumpInfo$.MortAssumpInfo2$q.Padd <- ifelse(q.Padd <= 1, q.Padd, 1)
      # Get joint life probabilities
      assumpInfo$q_x.Padd <- assumpInfo$.MortAssumpInfo1$q.Padd
      assumpInfo$q_y.Padd <- assumpInfo$.MortAssumpInfo2$q.Padd
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "MortAssump.2L",
   definition = function(object, cov, plan, assumpInfo = list(), projStartDate = NULL) {
      if (is.null(projStartDate)) {
         projStartDate <- GetIssDate(cov)
      }
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo, projStartDate)
      return(assumpInfo)
   }
)

GetLifeProb.2L <- function(q_x, q_y) {
   len <- min(length(q_x), length(q_y))
   length(q_x) <- len
   length(q_y) <- len
   p_x <- 1 - q_x
   p_y <- 1 - q_y
   t_p_x <- cumprod(p_x)
   t_p_y <- cumprod(p_y)
   jl.t_p_xy <- t_p_x * t_p_y
   ls.t_p_xy <- t_p_x + t_p_y - jl.t_p_xy
   ls.p_xy <- ls.t_p_xy / Rgogo::ShiftRight(ls.t_p_xy, positions = 1, filler = 1)
   ls.q_xy <- 1 - ls.p_xy
   jl.p_xy <- jl.t_p_xy / Rgogo::ShiftRight(jl.t_p_xy, positions = 1, filler = 1)
   jl.q_xy <- 1 - jl.p_xy
   df <- data.frame(
      q_x = q_x,
      p_x = p_x,
      t_p_x = t_p_x,
      q_y = q_y,
      p_y = p_y,
      t_p_y = t_p_y,
      jl.t_p_xy = jl.t_p_xy,
      jl.p_xy = jl.p_xy,
      jl.q_xy = jl.q_xy,
      ls.t_p_xy = ls.t_p_xy,
      ls.p_xy = ls.p_xy,
      ls.q_xy = ls.q_xy,
      stringsAsFactors = FALSE
   )
   return(df)
}


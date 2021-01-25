CalcNSP.TermLife <- function(q, i, n) {
   length(q) <- n
   if (length(i) == 1) {
      i <- rep(i, n)
   } else {
      length(i) <- n
   }
   p <- 1 - q
   pn <- ShiftRight(cumprod(p), positions = 1, filler = 1)
   v <- cumprod(1 / (1 + i))
   nsp <- sum(pn * q * v)
   return(nsp)
}

CalcNSP.WholeLife <- function(q, i) {
   return(CalcNSP.TermLife(q, i, length(q)))
}

CalcNSP.PureEnd <- function(q, i, n) {
   length(q) <- n
   if (length(i) == 1) {
      i <- rep(i, n)
   } else {
      length(i) <- n
   }
   pn <- cumprod(1 - q)
   v <- cumprod(1 / (1 + i))
   nsp <- pn[n] * v[n]
   return(nsp)
}

CalcNSP.End <- function(q, i, n) {
   nsp <- CalcNSP.TermLife(q, i, n) + CalcNSP.PureEnd(q, i, n)
   return(nsp)
}

CalcNSP.AnuImm <- function(q, i, n = NA_integer_) {
   if (!is.na(n)) {
      length(q) <- n
      if (length(i) == 1) {
         i <- rep(i, n)
      } else {
         length(i) <- n
      }
      pn <- cumprod(1 - q)
      v <- cumprod(1 / (1 + i))
      nsp <- sum(pn * v)
      return(nsp)
   } else {
      CalcNSP.AnuImm(q, i, length(q))
   }
}

CalcNSP.AnuDue <- function(q, i, n = NA_integer_) {
   if (!is.na(n)) {
      length(q) <- n
      if (length(i) == 1) {
         i <- rep(i, n)
      } else {
         length(i) <- n
      }
      pn <- ShiftRight(cumprod(1 - q), positions = 1, filler = 1)
      v <- ShiftRight(cumprod(1 / (1 + i)), position =1, filler = 1)
      nsp <- sum(pn * v)
      return(nsp)
   } else {
      CalcNSP.AnuDue(q, i, length(q))
   }
}

CalcNetPremRes.End <- function(q, i, n) {
   length(q) <- n
   p <- 1 - q
   if (length(i) == 1) {
      i <- rep(i, n)
   } else {
      length(i) <- n
   }
   ins <- CalcNSP.End(q, i, n)
   anu <- CalcNSP.AnuDue(q, i, n)
   np <- ins / anu
   res <- rep(0, n + 1)
   for (t in 1:n) {
      res[t + 1] <- ((res[t] + np) * (1 + i[t]) - q[t]) / p[t]
   }
   return(list(ins = ins, anu = anu, np = np, res = res))
}

CalcNetPremRes.TermLife <- function(q, i, n) {
   length(q) <- n
   p <- 1 - q
   if (length(i) == 1) {
      i <- rep(i, n)
   } else {
      length(i) <- n
   }
   ins <- CalcNSP.TermLife(q, i, n)
   anu <- CalcNSP.AnuDue(q, i, n)
   np <- ins / anu
   res <- rep(0, n + 1)
   for (t in 1:n) {
      res[t + 1] <- ((res[t] + np) * (1 + i[t]) - q[t]) / p[t]
   }
   return(list(ins = ins, anu = anu, np = np, res = res))
}

CalcCV.End <- function(q, i, n) {
   length(q) <- n
   p <- 1 - q
   if (length(i) == 1) {
      i <- rep(i, n)
   } else {
      length(i) <- n
   }
   result <- CalcNetPremRes.End(q, i, n)
   ins <- result$ins
   anu <- result$anu
   np <- result$np
   res <- result$res
   e1 <- 1.25 * min(np, 0.04) + 0.01
   sc <- rep(0, n + 1)
   for (t in 1:n) {
      q1 <- q[t:n]
      i1 <- i[t:n]
      sc[t] <- e1 * CalcNSP.AnuDue(q, i, n - t + 1)
   }
   cv <- res - sc
   result$e1 <- e1
   result$cv <-cv
   return(result)
}



GetYearlyTotal <- function(monthlyAmounts) {
   l <- length(monthlyAmounts)
   len = ceiling(l / 12)
   v <- as.vector(matrix(c(monthlyAmounts,rep(0, len * 12 - l)), nrow = len, ncol = 12, byrow = TRUE) %*% matrix(1, nrow = 12, ncol = 1))
   return(v)
}



GetYearStartValue <- function(monthlyValue) {
   v <- monthlyValue[(1:length(monthlyValue)) %% 12 == 1]
   return(v)
}


Shift <- function(v, positions, filler) {
   # Shift the elements of a vector by specified number of positions (positons).
   # Shift right if positions > 0; shift left if positions < 0.
   # Newly created positions are filled with the specified value (filler).
   if (positions > 0) {
      return (c(rep(filler, length.out = positions),v)[1:length(v)])
   } else {
      return (c(v, rep(filler, length.out = -positions))[(-positions + 1):(length(v) - positions)])
   }
}


ShiftRight <- function(v, positions, filler) {
   return(Shift(v, positions, filler))
}


ShiftLeft <- function(v, positions, filler) {
   return(Shift(v, -positions, filler))
}


LoanAmort <- function(loanAmt, amortMonths, intRate, intRateType) {
   lstResult <- list()
   if (as.numeric(intRateType) == 1){     # Stated rate
      ir <- intRate / 12
   } else {
      if (as.numeric(intRateType) == 2) {     # Effective annual rate
         ir <- (1 + intRate)^(1/12) - 1
      } else {
         stop("Unknown interest rate type.")
      }
   }
   i <- rep(ir, length.out = amortMonths)
   v <- cumprod((1 + i) ^ (-1))
   payment <- rep(loanAmt / sum(v), length.out = amortMonths)
   a1 <- cumprod(i + 1)
   a0 <- c(1, a1[1:(amortMonths-1)])
   bal <- loanAmt * a1 - cumsum(payment * a0)
   principalPayment <- c(loanAmt, bal[1:(length(bal)-1)]) - bal
   intPayment <- rep(payment, length.out = amortMonths) - principalPayment
   lstResult$LoanPayment <- payment      # Monthly loan payment
   lstResult$PrincipalPayment <- principalPayment
   lstResult$InterestPayment <- intPayment
   lstResult$LoanBalance <- c(loanAmt, bal)[1:amortMonths]     # Beginning-of-month loan balance
   return(lstResult)
}


CloneS4Object <- function(object){
   newObject <- new(Class = class(object))
   for (sn in slotNames(object)) {
      eval(parse(text = paste("newObject@",sn, " <- object@", sn, sep="")))
   }
   return(newObject)
}



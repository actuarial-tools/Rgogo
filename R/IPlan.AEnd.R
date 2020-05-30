#' @include IPlan.End.R
NULL

setClass(
   Class = "IPlan.AEnd",
   contains = "IPlan.End",
   slots = c(
      MatBenSchd = "numeric"
    )
)


IPlan.AEnd <- function(planId, covYears, premYears = NA, matBenSchd) {
   plan <- new(
      Class = "IPlan.AEnd",
      Id = as.character(planId),
      CovPeriod = c(CovYears = covYears),
      PremPeriod = c(PremYears = ifelse(is.na(premYears), covYears, premYears)),
      MatBenSchd = matBenSchd
   )
   return(plan)
}


setValidity(
   Class = "IPlan.AEnd",
   method = function(object) {
      err <- New.SysMessage()
      isValid = Validate(
         ValidatorGroup(
            Validator.Range(minValue = 0),
            Validator.Length(minLen = as.integer(object@CovPeriod["CovYears"]), maxLen = as.integer(object@CovPeriod["CovYears"]))
         ),
         object@MatBenSchd
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid maturity benefit schedule.  It must be a numeric vector of length equal to coverage period, and the values cannot be negative."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


setMethod(
   f = "GetMatBenSchd",
   signature = "IPlan.AEnd",
   definition = function(object, cov) {
      covMonths <- GetCovMonths(object, cov)
      m1 <- matrix(data = object@MatBenSchd, nrow = length(object@MatBenSchd), ncol = 1)
      m2 <- matrix(data = c(rep(0, 11), 1), nrow = 1, ncol = 12)
      unitMatBen <- c(0, FillZeroIfNA(as.vector(t(m1 %*% m2)), covMonths))
      return(unitMatBen)
   }
)


setMethod(
   f = "SetMatBenSchd<-",
   signature = "IPlan.AEnd",
   definition = function(object, value) {
      object@MatBenSchd <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetCVRateVector",
   signature = "IPlan.AEnd",
   definition = function(object, cov) {
      ## Note: partial maturity benefits are included in cash values
      covMonths <- GetCovMonths(object, cov)
      covYears <- ceiling(covMonths / 12)
      cvTable <- GetCVTable(object, cov)
      if (is.null(cvTable)) {return(rep(0, covMonths + 1))}
      # Get partial maturity benefit schedule
      mb <- GetMatBenSchd(object, cov)
      mb[length(mb)] <- 0   # Set final maturity benefit to zero for cash value calculation purpose
      cvFactor1 <- LookUp(cvTable, cov)[1:covYears]
      # Calculate cash value rate vector before paying partial maturity benefit
      cv1 <- matrix(data = cvFactor1, nrow = covYears)
      cv0 <- matrix(data = c(0, cvFactor1)[1:covYears], nrow = covYears)
      s <- matrix(data = seq(from = 1/12, to = 1, length.out = 12), ncol = 12)
      m <- cv0 %*% (1-s) + cv1 %*% s
      cvRate1 <- c(0, as.vector(t(m)))
      # Calculate cash value rate vector after paying partial maturity benefit
      cvFactor2 <- cvFactor1 - mb[seq(from = 13, to = length(mb), by = 12)]
      cv1 <- matrix(data = cvFactor2, nrow = covYears)
      cv0 <- matrix(data = c(0, cvFactor2)[1:covYears], nrow = covYears)
      m <- cv0 %*% (1-s) + cv1 %*% s
      cvRate2 <- c(0, as.vector(t(m)))
      # Adjust cash value rates during the year immediately following the payment of partial maturity
      for (mm in (1:length(mb))[mb > 0]) {
         idx <- (mm + 1):(mm + 12)
         cvRate1[idx] <- cvRate2[idx]
      }
      return(cvRate1)
   }
)


setMethod(
   f = "ProjMatBen",
   signature = "IPlan.AEnd",
   definition = function(object, cov, resultContainer) {
      matBen <- GetMatBenSchd(object, cov) * GetFaceAmt(cov)
      resultContainer$Proj.Ben.Mat <- matBen
      resultContainer %<>% AddProjection(projItem = "Ben.Mat", projValue = matBen)
      return(resultContainer)
   }
)


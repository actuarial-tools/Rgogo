#' @include IPlan.LT.R
NULL

setClass(
   Class = "IPlan.End",
   contains = "IPlan.LT",
   slots = c(
      CVTable = "character",
      SurChrgSchd = "numeric"
   )
)

setValidity(
   Class = "IPlan.End",
   method = function(object) {
      err <- New.SysMessage()
      # Validate cash value table
      v <- Validator.Names(hasNames = (length(object@CVTable) > 1))
      if (Validate(v, object@CVTable) != TRUE) {
         AddMessage(err) <- paste0(GetId(object), ": Invalid cash value table setting.")
      }
      # Validate @SurChrgSchd
      isValid <- Validate(Validator.Range(minValue = 0, maxValue = 1), object@SurChrgSchd)
      if (isValid != TRUE) {
         AddMessage(err) <- "Invalid surrender charge schedule.  The rates must be between 0 and 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.End <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                      premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                      polFee = numeric(0), premTaxRate = numeric(0L), cvTable = character(0L), surChrgSchd = numeric(0L),
                      commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                      rein = character(0L), id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   if (is.na(premYears) & is.na(premToAge)) {
      premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   } else {
      premPeriod <- c(PremYears = premYears, PremToAge = as.integer(premToAge))
   }
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.End",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               CVTable = cvTable,
               SurChrgSchd = surChrgSchd,
               CommSchd = commSchd,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnCommSchd = ovrdOnCommSchd,
               PremTaxRate = premTaxRate,
               Rein = rein,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetCVTable",
   signature = "IPlan.End",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@CVTable)
      }
      if (length(object@CVTable) == 0) {
         return(NULL)
      }
      if (length(object@CVTable) == 1) {
         tblId <- object@CVTable
      } else {
         tblId <- object@CVTable[GetRiskClass(object, cov)]
      }
      tblId <- ifelse(startsWith(tblId, "CV."), tblId, paste0("CV.", tblId))
      return(eval(expr = parse(text = tblId)))
   }
)

setMethod(
   f = "SetCVTable<-",
   signature = "IPlan.End",
   definition = function(object, value) {
      object@CVTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCVRateVector",
   signature = "IPlan.End",
   definition = function(object, cov) {
      covMonths <- GetCovMonths(object, cov)
      covYears <- ceiling(covMonths / 12)
      cvTable <- GetCVTable(object, cov)
      if (is.null(cvTable)) {return(rep(0, covMonths))}
      cvFactor <- LookUp(cvTable, cov)[1:covYears]     # Cash value factors at the end of policy years
      cv1 <- matrix(data = cvFactor, nrow = covYears)     # Cash value factors at the end of policy years, in 1 by N matrix
      cv0 <- matrix(data = c(0, cvFactor)[1:covYears], nrow = covYears)     # Cash value factors at the beginning of policy years, in 1 by N matrix
      s <- matrix(data = seq(from = 1/12, to = 1, length.out = 12), ncol = 12)
      m <- cv0 %*% (1-s) + cv1 %*% s
      cvRate <- as.vector(t(m))[1:covMonths]
      return(cvRate)
   }
)

setMethod(
   f = "GetSurChrgSchd",
   signature = "IPlan.End",
   definition = function(object, cov = NULL) {
      if (is.null(cov)) {
         return(object@SurChrgSchd)
      } else if (length(object@SurChrgSchd) == 0) {
         return(rep(0, length.out = GetCovMonths(object, cov)))
      } else if (length(object@SurChrgSchd) == 1) {
         return(rep(object@SurChrgSchd, length.out = GetCovMonths(object, cov)))
      } else {
         return(FillTail(rep(object@SurChrgSchd, each = 12), filler = 0, len = GetCovMonths(object, cov)))
      }
   }
)

setMethod(
   f = "SetSurChrgSchd<-",
   signature = "IPlan.End",
   definition = function(object, value) {
      object@SurChrgSchd <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjMatBen",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      matBen <- c(rep(0, covMonths - 1), GetFaceAmt(cov))
      resultContainer$Proj$Ben.Mat <- matBen
      return(resultContainer)
   }
)

setMethod(
   f = "ProjCV",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer){
      projCV <- GetFaceAmt(cov) * GetCVRateVector(object, cov)
      resultContainer$Proj$CV <- projCV
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurChrg",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      surChrg <- resultContainer$Proj$CV * GetSurChrgSchd(object, cov)
      if (!all(surChrg == 0)) {
         resultContainer$Proj$Chrg.Sur <- surChrg
      }
      return(resultContainer)
   }
)

setMethod(
   f = "ProjSurBen",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      if (is.null(resultContainer$Proj$Chrg.Sur)) {
         surBen <- resultContainer$Proj$CV
      } else {
         surBen <- resultContainer$Proj$CV - resultContainer$Proj$Chrg.Sur
      }
      resultContainer$Proj$Ben.Sur <- ifelse(surBen > 0, surBen, 0)
      return(resultContainer)
   }
)

setMethod(
   f = "Project",
   signature = "IPlan.End",
   definition = function(object, cov, resultContainer) {
      resultContainer <- NewProjection(resultContainer, cov, object)
      resultContainer <- ProjPrem(object, cov, resultContainer)
      resultContainer <- ProjComm(object, cov, resultContainer)
      resultContainer <- ProjDthBen(object, cov, resultContainer)
      resultContainer <- ProjMatBen(object, cov, resultContainer)
      resultContainer <- ProjCV(object, cov, resultContainer)
      resultContainer <- ProjSurBen(object, cov, resultContainer)
      resultContainer <- ProjRein(object, cov, resultContainer)
      return(resultContainer)
   }
)




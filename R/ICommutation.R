setClass(
   Class = "ICommutation",
   contains = "VIRTUAL"
)

setMethod(
   f = "Get.q",
   signature = "ICommutation",
   definition = function(object, entryAge = 0) {
      stop("Get.q method must be implemented by a class extending ICommutation.")
      # This method returns a numeric vector containing the mortality rates for ages starting from
      # 'startAge' to the final age of a motality model.  The vector must have name attribute in which
      # the element names are the attained ages.
   }
)

setMethod(
   f = "Get.i",
   signature = "ICommutation",
   definition = function(object, len = NA_integer_) {
      stop("Get.i method must be implemented by a class extending ICommutation.")
      # This method returns a numeric vector of length equal to len, containing the effective interest rate
      # for the n-year period.
   }
)

setMethod(
   f = "Get.p",
   signature = "ICommutation",
   definition = function(object, entryAge = 0) {
      return(1 - Get.q(object, entryAge))
   }
)

setMethod(
   f = "Get.pn",
   signature = "ICommutation",
   definition = function(object, entryAge = 0) {
      p <- 1 - Get.q(object, entryAge)
      return(cumprod(p))
   }
)

setMethod(
   f = "Get.l",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      pn <- Get.pn(object, entryAge)
      l <- radix * ShiftRight(pn, positions = 1, filler = 1)
      names(l) <- as.character(seq_along(l) + entryAge - 1)
      return(l)
   }
)

setMethod(
   f = "Get.d",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      l <- Get.l(object, entryAge, radix)
      d <- l * Get.q(object, entryAge)
      names(d) <- as.character(seq_along(d) + entryAge - 1)
      return(d)
   }
)

setMethod(
   f = "Get.v",
   signature = "ICommutation",
   definition = function(object, len = NA_integer_) {
      i <- Get.i(object, len)
      return(cumprod(1 / (1 + i)))
   }
)

setMethod(
   f = "Get.D",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      l <- Get.l(object, entryAge, radix)
      v <- ShiftRight(Get.v(object, len = length(l)), positions = 1, filler = 1)
      return(l * v)
   }
)

setMethod(
   f = "Get.C",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      d <- Get.d(object, entryAge, radix)
      v <- Get.v(object, len = length(d))
      return(d * v)
   }
)

setMethod(
   f = "Get.M",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      C <- Get.C(object, entryAge, radix)
      M <- rev(cumsum(rev(C)))
      return(M)
   }
)

setMethod(
   f = "Get.R",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      M <- Get.M(object, entryAge, radix)
      R <- rev(cumsum(rev(M)))
      return(R)
   }
)

setMethod(
   f = "Get.N",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      D <- Get.D(object, entryAge, radix)
      N <- rev(cumsum(rev(D)))
      return(N)
   }
)

setMethod(
   f = "Get.S",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1) {
      N <- Get.N(object, entryAge, radix)
      S <- rev(cumsum(rev(N)))
      return(S)
   }
)

setMethod(
   f = "Get.qm",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, m, method = "ud") {
      stopifnot(m %in% c(1, 2, 4, 12))
      stopifnot(method %in% c("ud", "cf", "ba"))
      q <- Get.q(object, entryAge)
      if (m == 1) {
         return(q)
      } else {
         qm <- Convert_qx(q, m, method)
         names(qm) <- unlist(lapply(as.list(seq_along(q) + entryAge - 1), function(x){paste0(x,".", 1:m)}))
         return(qm)
      }
   }
)

setMethod(
   f = "Get.im",
   signature = "ICommutation",
   definition = function(object, m, len) {
      stopifnot(m %in% c(1, 2, 4,12))
      im <- rep((1 + Get.i(object, len))^(1/m) - 1, each = m)
      return(im)
   }
)

setMethod(
   f = "Get.vm",
   signature = "ICommutation",
   definition = function(object, m, len) {
      stopifnot(m %in% c(1, 2, 4,12))
      im <- Get.im(object, m, len)
      return(cumprod(1 / (1 + im)))
   }
)

setMethod(
   f = "Get.A",
   signature = "ICommutation",
   definition = function(object, entryAge = 0) {
      return(Get.M(object, entryAge) / Get.D(object, entryAge))
   }
)

setMethod(
   f = "Get.a",
   signature = "ICommutation",
   definition = function(object, entryAge = 0) {
      return(Get.N(object, entryAge) / Get.D(object, entryAge))
   }
)

setMethod(
   f = "Get.lm",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1, m, method = "ud") {
      qm <- Get.qm(object, entryAge, m, method)
      pn <- cumprod(1 - qm)
      lm <- radix * ShiftRight(pn, positions = 1, filler = 1)
      names(lm) <- names(qm)
      return(lm)
   }
)

setMethod(
   f = "Get.Dm",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1, m, method = "ud") {
      lm <- Get.lm(object, entryAge, radix, m, method)
      vm <- ShiftRight(Get.vm(object, m, len = length(lm) / m), positions = 1, filler = 1)
      Dm <- lm * vm
      names(Dm) <- names(lm)
      return(Dm)
   }
)

setMethod(
   f = "Get.Nm",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, radix = 1, m, method = "ud") {
      Dm <- Get.Dm(object, entryAge, radix, m, method)
      Nm <- rev(cumsum(rev(Dm)))
      return(Nm)
   }
)


setMethod(
   f = "Get.am",
   signature = "ICommutation",
   definition = function(object, entryAge = 0, m = 1, method = "ud") {
      if (m == 1) {
         return(Get.a(object, entryAge))
      } else {
         return((1 / m) * Get.Nm(object, entryAge, m = m, method = method) / Get.Dm(object, entryAge, m = m, method = method))
      }
   }
)

setMethod(
   f = "Get.ag",
   signature = c("ICommutation", "numeric"),
   definition = function(object, crtnYears, entryAge) {
      l_agx <- Get.l(object, entryAge = entryAge)
      l_agx[1:crtnYears] <- l_agx[1]
      v <- ShiftRight(Get.v(object, len = length(l_agx)), positions = 1, filler = 1)
      D_agx <- l_agx * v
      N_agx <- rev(cumsum(rev(D_agx)))
      a <- N_agx / Get.D(object, entryAge = entryAge)
      return(a)
   }
)

setMethod(
   f = "Get.amg",
   signature = c("ICommutation", "numeric"),
   definition = function(object, crtnYears, entryAge, m = 1, method = "ud") {
      stopifnot(m %in% c(1, 2, 4, 12))
      if (m == 1) {
         return(Get.ag(object, crtnYears, entryAge))
      } else {
         l_agx <- Get.lm(object, entryAge = entryAge, m = m, method = method)
         l_agx[1:as.integer(crtnYears * m)] <- l_agx[1]
         v <- ShiftRight(Get.vm(object, m, len = length(l_agx) / m), positions = 1, filler = 1)
         D_agx <- l_agx * v
         N_agx <- rev(cumsum(rev(D_agx)))
         a <- (1 / m) * N_agx / Get.Dm(object, entryAge = entryAge, m = m, method = method)
         return(a)
      }
   }
)




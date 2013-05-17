descStats <- function(x, na.rm = TRUE, trim = NULL, skew = FALSE, byrow = FALSE, digits = getOption("digits")) {
  fun <- function(x) {
    stopifnot(is.numeric(x))
    if (na.rm) x <- x[!is.na(x)]
    n <- length(x)
    mean <- sum(x) / n
    dev <- x - mean
    dev_ss <- sum(dev^2L)
    sd <- sqrt(dev_ss / (n-1))
    se <- sqrt(sd / n)
    median <- local({
      half <- (n + 1L)%/%2L
      if (n%%2L == 1L) 
        return(.Internal(sort(x, partial = half))[half])
      else {
        return(.Internal(mean(.Internal(sort(x, partial = half + 0L:1L))[half + 0L:1L])))
      }
    })
    min <- min(x)
    max <- max(x)
    range <- max - min
    stats <- c(n, mean, se, sd, median, min, max, range)
    if (!is.null(trim)) {
      if (trim >= 0.5)
        trimmed <- median
      else {
        trimmed <- local({
          lo <- floor(n * trim) + 1
          hi <- n + 1 - lo
          x <- .Internal(sort(x, partial = unique(c(lo, hi))))[lo:hi]
          return(sum(x) / n)
        })
      }
      stats <- append(stats, trimmed, 2)
    }
    if (skew) {      
      skewness <- (sum(dev^3L)/n) / (dev_ss/n)^1.5
      kurtosis <- n * sum(dev^4L) / (dev_ss^2L)
      stats <- c(stats, skewness, kurtosis)
    }
    return(stats)
  }
  if (!is.matrix(x)) x <- as.matrix(x)
  if(byrow) x <- t.default(x)
  nstats <- c("n", "mean", "se", "sd", "median", "min", "max", "range")
  if (skew) nstats <- c(nstats, "skewness", "kurtosis")
  if (!is.null(trim)) nstats <- append(nstats, "trimmed", 2)
  out <- matrix(
    unlist(
      lapply(seq_len(ncol(x)), function(i) fun(x[,i]))
    ), ncol = length(nstats), nrow = ncol(x),
    dimnames = list(colnames(x), nstats), byrow=TRUE)
  return(round(out, digits=digits))
}

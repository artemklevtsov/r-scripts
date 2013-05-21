descStats <- function(x, na.rm = TRUE, trim = NULL, skew = FALSE, byrow = FALSE, digits = getOption("digits")) {
  fun <- function(x) {
    stopifnot(is.numeric(x))
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    mean <- sum(x) / n
    dev <- x - mean
    dev_ss <- sum(dev^2L)
    sd <- sqrt(dev_ss / (n - 1))
    se <- sqrt(sd / n)
    half <- (n + 1L)%/%2L
    if (n %% 2L == 1L)
      median <- .Internal(sort(x, partial = half))[half]
    else
      median <- .Internal(mean(.Internal(sort(x, partial = half + 0L:1L))[half + 0L:1L]))
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
      skewness <- (sum(dev^3L) / n) / (dev_ss / n)^1.5
      kurtosis <- n * sum(dev^4L) / (dev_ss^2L)
      stats <- c(stats, skewness, kurtosis)
    }
    return(stats)
  }
  var.names <- colnames(x)
  n.vars <- ncol(x)
  if (is.data.frame(x)) {
    for (i in seq_len(n.vars)) {
      if (!is.numeric(x[, i])) {
        x <- x[, -i]
        warning(paste("Variable \"", colnames(x)[i], "\"was removed from data.frame."))
      }
    }
  }
  if (!is.matrix(x))
    x <- as.matrix(x)
  if (byrow)
    x <- t.default(x)
  stat.nanmes <- c("n", "mean", "se", "sd", "median", "min", "max", "range")
  if (skew)
    stat.nanmes <- c(stat.nanmes, "skewness", "kurtosis")
  if (!is.null(trim))
    stat.nanmes <- append(stat.nanmes, "trimmed", 2)
  result <- matrix(numeric(1), ncol = length(stat.nanmes), nrow = n.vars,
                   dimnames = list(var.names, stat.nanmes), byrow = TRUE)
  for (i in seq_len(n.vars))
    result[i, ] <- fun(x[, i])
  return(signif(result, digits = digits))
}

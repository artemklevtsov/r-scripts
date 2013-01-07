descStats <- function(x, skew = FALSE, byrow = FALSE) {
  x <- as.matrix(x)
  if(byrow) x <- t(x)
  stats <- c("n", "min", "max", "range", "mean", "median", "sd")
  if (skew) {
    library(moments)
    stats <- c(stats, "skewness", "kurtosis")
  }
  n <- function(x, ...) sum(!is.na(x), ...)
  range <- function(x, ...) max(x, ...) - min(x, ...)
  describe <- function(x) {
    result <- vapply(stats, function(fun) eval(call(fun, x, na.rm=TRUE)), FUN.VALUE=numeric(1))
  }
  out <- t(vapply(seq_len(ncol(x)), function(i) describe(x[,i]), FUN.VALUE=numeric(length(stats))))
  return(out)
}
descStats <- function(x, na.rm = TRUE, trim = NULL, skew = FALSE, byrow = FALSE) {
  x <- as.matrix(x)
  if(byrow) x <- t(x)
  stats <- c("n", "min", "max", "range", "mean", "median", "sd")
  if (skew) {
    library(moments)
    stats <- c(stats, "skewness", "kurtosis")
  }
  if (!is.null(trim)) {
    stats <- append(stats, "trimmed", which(stats == "mean"))
    trimmed <- function(x, na.rm=FALSE) mean(x, na.rm=na.rm, trim=trim)
  }
  n <- function(x, na.rm=FALSE) sum(!is.na(x), na.rm=na.rm)
  range <- function(x, na.rm=FALSE) max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
  describe <- function(x) {
    result <- vapply(stats, function(fun) eval(call(fun, x, na.rm=na.rm)), FUN.VALUE=numeric(1))
  }
  out <- t(vapply(seq_len(ncol(x)), function(i) describe(x[,i]), FUN.VALUE=numeric(length(stats))))
  return(out)
}
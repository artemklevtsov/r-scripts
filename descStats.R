descStats <- function(x, stats = c("n", "min", "max", "range", "mean", "median", "sd"), ...) {
  n <- function(y, ...) sum(!is.na(y), ...)
  range <- function(y, ...) max(y, ...) - min(y, ...)
  if (is.vector(x)) {
    result <- vapply(stats, function(z) eval(call(z, x, ...), FUN.VALUE=numeric(1))
  }
  if (is.matrix(x) || is.data.frame(x)) {
    result <- apply(x, 2, function(x) vapply(stats, function(z) eval(call(z, x, ...), FUN.VALUE=numeric(1)))
  }
  return(result)
}
descStats <- function(x, stats = c("n", "min", "max", "range", "mean", "median", "sd")) {
  n <- function(x, ...) sum(!is.na(x), ...)
  range <- function(x, ...) max(x, ...) - min(x, ...)
  fun <- function(x) {
    result <- vapply(stats, function(z) eval(call(z, x, na.rm=TRUE)), FUN.VALUE=numeric(1))
  }
  if (is.vector(x)) {
    result <- fun(x)
  }
  if (is.matrix(x) || is.data.frame(x)) {
    result <- t(apply(x, 2, fun))
  }
  return(result)
}
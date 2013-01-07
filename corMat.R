corMat <- function(x, digits=getOption("digits"), ...) {
  ifelse (is.data.frame(x),  mat <- data.matrix(x), mat <- x)
  cor.mat <- cor(mat, ...)
  index <- combn(ncol(mat), 2)
  pvals <- mapply(function(x, y) cor.test(mat[, x], mat[, y], ...)$p.value, index[1, ], index[2, ])
  cor.mat[lower.tri(cor.mat)] <- pvals
  diag(cor.mat) <- NA
  cor.mat <- round(cor.mat, digits=digits)
  return(cor.mat)
}
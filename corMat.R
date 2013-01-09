corMat <- function(x, method = c("pearson", "kendall", "spearman"), digits=getOption("digits")) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  stopifnot(ncol(x) > 1L)
  ifelse (is.data.frame(x),  mat <- data.matrix(x), mat <- x)
  method <- match.arg(method)
  cor.mat <- cor(mat, method = method)
  index <- combn(ncol(mat), 2)
  pvals <- mapply(function(x, y) cor.test(mat[, x], mat[, y], method = method)$p.value, index[1, ], index[2, ])
  cor.mat[lower.tri(cor.mat)] <- pvals
  diag(cor.mat) <- NA
  round(cor.mat, digits=digits)
}
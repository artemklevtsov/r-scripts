corMat <- function(x, digits=getOption("digits"), method = c("pearson", "kendall", "spearman")) {
  ifelse (is.data.frame(x),  mat <- data.matrix(x), mat <- x)
  method <- match.arg(method)
  cor.mat <- cor(mat, method = method)
  index <- combn(ncol(mat), 2)
  pvals <- mapply(function(x, y) cor.test(mat[, x], mat[, y], method = method)$p.value, index[1, ], index[2, ])
  cor.mat[lower.tri(cor.mat)] <- pvals
  diag(cor.mat) <- NA
  cor.mat <- round(cor.mat, digits=digits)
  return(cor.mat)
}
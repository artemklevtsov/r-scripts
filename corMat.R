corMat <- function(x, method = c("pearson", "kendall", "spearman"), digits = getOption("digits")) {
    stopifnot(is.matrix(x) || is.data.frame(x))
    stopifnot(ncol(x) > 1L)
    ifelse(is.data.frame(x), mat <- data.matrix(x), mat <- x)
    method <- match.arg(method)
    cor.mat <- cor(mat, method = method)
    index <- combn(ncol(mat), 2)
    p.vals <- numeric(ncol(index))
    for (i in seq_along(p.vals))
        p.vals[i] <- cor.test(mat[, index[1, i]],mat[, index[2, i]], method = method)$p.value
    cor.mat[lower.tri(cor.mat)] <- p.vals
    diag(cor.mat) <- NA
    return(signif(cor.mat, digits = digits))
}
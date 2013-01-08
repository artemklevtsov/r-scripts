descStats <- function(x, na.rm = TRUE, trim = NULL, skew = FALSE, byrow = FALSE, digits = getOption("digits")) {
  fun <- function(x) {
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    mean <- sum(x)/n # mean
    dev <- x-mean
    dev_s2 <- sum(dev^2L)
    sd <- sqrt(dev_s2/(n-1)) # sd
    se <- sqrt(sd/n) # se
    median <- local({ # median
      half <- (n + 1L)%/%2L
      if (n%%2L == 1L) 
        .Internal(sort(x, partial = half))[half]
      else {
        mean(.Internal(sort(x, partial = half + 0L:1L))[half + 0L:1L])
      }
    })
    min <- min(x)
    max <- max(x)
    range <- max - min
    stats <- c(n, mean, se, sd, median, min, max, range)
    if (!is.null(trim)) {
      trimmed <- mean.default(x, trim=trim)
      stats <- append(stats, trimmed, 2)
    }
    if (skew) {      
      skewness <- (sum(dev^3L)/n) / (dev_s2/n)^1.5
      stats <- c(stats, skewness)
      
      kurtosis <- n * sum(dev^4L) / (dev_s2^2L)
      stats <- c(stats, kurtosis)
    }
    stats
  }
  if (!is.matrix(x)) x <- as.matrix(x)
  if(byrow) x <- t(x)
  nstats <- c("n", "mean", "se", "sd", "median", "min", "max", "range")
  if (skew) nstats <- c(nstats, "skewness", "kurtosis")
  if (!is.null(trim)) nstats <- append(nstats, "trimmed", which(nstats == "mean"))
  
  #out <- t(vapply(seq_len(ncol(x)), function(i) fun(x[,i]), FUN.VALUE=numeric(nstats)))
  out <- matrix(0.0, ncol = length(nstats), nrow = ncol(x), dimnames=list(colnames(x), nstats))
  for(i in 1:ncol(x)) {
    out[i,] <- fun(x[,i])    
  }
  round(out, digits=digits)
}
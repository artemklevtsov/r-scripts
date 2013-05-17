importCSV <- function(path=getwd()) {
  files <- list.files(path=path, pattern="*.csv", full.names=TRUE) # file names
  vnames <- strsplit(x=files, split=c("/|\\."))
  n <- length(x=vnames[[1]]) # length data.names vector
  vnames <- vapply(X=vnames, FUN="[[", n-1, FUN.VALUE=character(1)) # variable names
  vnames <- gsub(pattern="[-\ ]", replacement=".", x=vnames) # replace "-" to "."
  result <- lapply(X=files, FUN=read.csv, header=TRUE, sep=";", dec=",")
  names(x=result) <- vnames
  return(result)
}

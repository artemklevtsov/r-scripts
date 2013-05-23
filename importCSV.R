importCSV <- function(path = getwd()) {
  files <- list.files(path = path, pattern = "*.csv", full.names = TRUE) # file names
  vnames <- strsplit(files, split = c("/|\\."))
  n <- length(vnames[[1]]) # length data.names vector
  vnames <- vapply(vnames, "[[", n - 1, FUN.VALUE = character(1)) # variable names
  vnames <- gsub(pattern = "[-\ ]", replacement = ".", vnames) # replace "-" to "."
  result <- lapply(files, read.csv, header = TRUE, sep = ";", dec = ",")
  names(result) <- vnames
  return(result)
}

proftable <- function(x, ...) {
    UseMethod("proftable")
}

proftable.default <- function(filename, lines = 10) {
    con <- file(filename, "rt")
    on.exit(close(con))
    profdata <- readLines(con)
    interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
    profdata <- profdata[-1L]
    filelines <- grep("^#File [0-9]+: ", profdata)
    if (length(filelines)) {
        files <- profdata[filelines]
        filenums <- as.integer(gsub("^#File ([0-9]+): .*", "\\1", files))
        filenames <- gsub("^#File [0-9]+: ", "", files)
        profdata <- profdata[-filelines]
    }
    ncalls <- length(profdata)
    total.time <- interval * ncalls
    profdata <- gsub("\\\"| $", "", profdata)
    stacktable <- as.data.frame(table(profdata) / ncalls * 100, stringsAsFactors = FALSE)
    calls <- strsplit(stacktable$profdata, " ")
    calls <- lapply(calls, function(x) rev(x))
    min.len <- min(vapply(calls, length, FUN.VALUE = numeric(1)))
    parent.call <- unlist(lapply(seq_len(min.len), function(i) Reduce(intersect, lapply(unique(calls), "[[", i))))
    calls <- lapply(calls, function(x) setdiff(x, parent.call))
    stacktable$profdata <- vapply(calls, function(x) paste(x, collapse = " > "), FUN.VALUE = character(1))
    stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
    colnames(stacktable) <- c("PctTime", "Call")
    rownames(stacktable) <- NULL
    stacktable <- head(stacktable, lines)
    if (length(parent.call) > 0)
        parent.call <- paste(parent.call, collapse = " > ")
    else
        parent.call <- "None"
    frac <- sum(stacktable$PctTime)
    result <- list(data = profdata, table = stacktable, parent.call = parent.call, interval = interval, total.time = total.time, files = filenames, total.pct.time = frac)
    class(result) <- "proftable"
    return(result)
}
 
print.proftable <- function(x) {
    print(x$table, row.names=FALSE, right=FALSE, digits=3)
    cat("\nFiles:\n")
    cat(paste(x$files, collapse="\n"))
    cat("\n\n")
    cat(paste("Parent Call:", x$parent.call))
    cat("\n\n")
    cat(paste("Total Time:", x$total.time, "seconds"))
    cat("\n")
    cat(paste0("Percent of run time represented: ", format(x$total.pct.time, digits = 3), "%"))
}

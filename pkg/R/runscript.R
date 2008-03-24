runscript <- function(x, fmt="ch%02d.R", package="FinTS", 
                      subdir="scripts", lib.loc=NULL){ 
    # chnames <- c(...) # chapter names
    # if (missing(x)) x <- match(select.list(chnames), chnames)
    x <- as.numeric(x)
#    s <- system.file("scripts", package = "FinTS")
    s <- system.file(subdir, package = package, lib.loc=lib.loc)
#    ch <- sprintf("ch%02d.R", x)
    ch <- sprintf(fmt, x)
    Ch <- paste(s, ch, sep="/")
    source(Ch, echo = TRUE)
}

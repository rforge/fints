runscript <- function(x, method=c('run', 'copy', 'view', 'dir'),  
                      ask = TRUE, fmt="ch%02d.R", package="FinTS", 
                      subdir="scripts", lib.loc=NULL){
##
## 1.  Set up
##  
  mtd <- match.arg(method) 
    # chnames <- c(...) # chapter names
    # if (missing(x)) x <- match(select.list(chnames), chnames)
    x <- as.numeric(x)
#    s <- system.file("scripts", package = "FinTS")
    s <- system.file(subdir, package = package, lib.loc=lib.loc)
#    ch <- sprintf("ch%02d.R", x)
    ch <- sprintf(fmt, x)
    Ch <- paste(s, ch, sep="/")
##
## 2.  method == 'dir'
##
  if(method=='dir')return(Ch) 
##
## 3.  method == 'run'
##
  if(method=='run'){
    if(ask){
      op <- par(ask=TRUE) 
      on.exit(par(op))
    } 
    source(Ch, echo = TRUE)
    return(invisible(Ch)) 
  }
##
## 4.  method == 'view'
##
  ch. <- readLines(Ch)
#  
  if(method=='view'){
    cat(ch., sep="\n") 
    return(invisible(Ch)) 
  }
##
## 5.  method == 'copy'
##
  writeLines(ch., ch)
  return(invisible(Ch)) 
}

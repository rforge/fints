eacf <- function (x,ar.max=7,ma.max=13){
#
#  PROGRAMMED BY K.S. CHAN, DEPARTMENT OF STATISTICS AND ACTUARIAL SCIENCE,
#  UNIVERSITY OF IOWA.
#
#  DATE: 4/2001
#  Compute the extended sample acf (ESACF) for the time series stored in z.
#  The matrix of ESACF with the AR order up to ar.max and the MA order
#  up to ma.max is stored in the matrix EACFM.
#  The default values for NAR and NMA are 7 and 13 respectively.
#  
#  The print method for class 'eacf' prints a coded ESACF table with
#  significant values denoted by 'X' and nosignificant values by
#  'O', significance level being 5%.
#
#  Output:  numeric matrix of class 'esacf'
#
##
## 1.  Prep x
##  
  z <- as.numeric(x)
  lag1<-function(z,lag=1){c(rep(NA,lag),z[1:(length(z)-lag)])}
##
## 2.  Define internal functions
##  
  reupm<-function(m1,nrow,ncol){
    k<-ncol-1
    m2<-NULL
    for (i in 1:k){
      i1<-i+1
      work<-lag1(m1[,i])
      work[1]<--1
      temp<-m1[,i1]-work*m1[i1,i1]/m1[i,i]
      temp[i1]<-0
      m2<-cbind(m2,temp)
    }
    m2
  }
  ceascf<-function(m,cov1,nar,ncol,count,ncov,z,zm){
    result<-0*seq(1,nar+1)
    result[1]<-cov1[ncov+count]
    for (i in 1:nar) {
      temp<-cbind(z[-(1:i)],zm[-(1:i),1:i])%*%c(1,-m[1:i,i])
      result[i+1]<-acf(temp,plot=FALSE,lag.max=count)$acf[count+1]
    }
    result
  }
##
## 3.  Compute ... 
##  
  ar.max<-ar.max+1
  ma.max<-ma.max+1
  nar<-ar.max-1
  nma<-ma.max
  ncov<-nar+nma+2
  nrow<-nar+nma+1
  ncol<-nrow-1
  z<-z-mean(z)
  zm<-NULL
  for(i in 1:nar) zm<-cbind(zm,lag1(z,lag=i))
#  
  cov1<-acf(z,lag.max=ncov,plot=FALSE)$acf
  cov1<-c(rev(cov1[-1]),cov1)
  ncov<-ncov+1
  m1<-matrix(0,ncol=ncol,nrow=nrow)
  for(i in 1:ncol)
    m1[1:i,i]<- ar.ols(z,order.max=i,aic=FALSE,demean=FALSE,
                       intercept=FALSE)$ar
  eacfm<-NULL
  for (i in 1:nma) {
    m2<-reupm(m1=m1,nrow=nrow,ncol=ncol)
    ncol<-ncol-1
    eacfm<-cbind(eacfm, ceascf(m2,cov1,nar,ncol,i,ncov,z,zm))
    m1<-m2
  }

  dimnames(eacfm) <- list(0:(ar.max-1), 0:(ma.max-1))
  attr(eacfm, "nobs") <- length(z)
  class(eacfm) <- "eacf" 
  eacfm
}

print.eacf <- function(x, symbols=c("O", "X"), ...){
##
## 1.  If 'x' is not a matrix, print.default(...)
##  
  nam <- dim(x)
  if(length(nam)!=2)
    return(print.default(x, ...))
##
## 2.  If 'symbols' is character, translate to characters
##
  nobs <- attr(x, 'nobs') 
  if(is.character(symbols) && (length(symbols)>1)
     && is.numeric(nobs) && (nobs[1]>sum(nam)) ){
    EACF <- array(NA, dim=nam)
#
    work <- nobs-(0:(nam[1]-1))
    nch <- nchar(symbols)
    cat('AR/MA\n')
#
    Nch <- max(nch)
    symb <- paste(rep(" ", length=Nch-nch), symbols, sep="") 
    for(im in 1:nam[2]){
      work <- work-1
      EACF[, im] <- symb[1+(x[, im]>2/sqrt(work))]
    }
#
    cat('AR/MA\n')
    mao <- as.character(1:nam[2])
    colps <- " "
    if(nam[2]>9){
      mao <- c(paste(" ", mao[1:9], sep=""), 10:nam[2])
      colps <- "  "
    }
    nUnd <- (nchar(colps)+1)*nam[2]
    cat(paste(rep(" ", length=nUnd/2.3), collapse=""), "MA order: q\n")
    cat("___", paste(rep("_", length=nUnd), collapse=""), "\n")
    cat(" p |", paste(mao, collapse=" "), "\n")
    cat("___", paste(rep("_", length=nUnd), collapse=""), "\n")
    for(ia in 1:nam[1]){
      ar. <- as.character(ia)
      if(ia<10)ar. <- paste(" ", ar., sep="")
      cat(ar., "| ", paste(EACF[ia, ], collapse=colps), "\n")
    }
#
    return(invisible(EACF))
  }
##
## 3.  If 'symbols' is a positive integer, round 
##
  if(is.numeric(symbols) && (length(symbols)==1)
     && (symbols>0) && (symbols%%1 == 0) && is.numeric(x) ){ 
    eacf <- round(x, symbols)
    cat('AR/MA\n')
    return(print.default(eacf))
  }
##
## 4.  Else print.default(...)
##
  print.default(x, ...)
}

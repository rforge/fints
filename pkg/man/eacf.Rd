\name{eacf}
\alias{eacf}
\alias{print.eacf}
\title{ Extended Autocorrelation Function }
\description{
  Compute the Extended Autocorrelation Function 
}
\usage{
eacf(x, ar.max=7, ma.max=13)
\method{print}{eacf}(x, symbols=c("O", "X"), ...)
}
\arguments{
  \item{x}{
    for 'eacf':  a univariate time series or a numeric vector to
    interpret as a univariate time series.

    for 'print.eacf':  a numeric matrix of class 'eacf'  
  }
  \item{ar.max, ma.max}{
    the maximum number of autoregressive and moving average lags,
    respectively, for the'eacf' table.  
  }
  \item{symbols}{
    if a character vector of length 2, 'print.eacf' prints a character
    matrix of the same dimensions as 'x' with symbol[2] whenever abs(x)
    is statistically significant at the approximate 0.05 level, as
    judged by exceeding 2/sqrt(nobs-p-q-1), where p and q are the AR and
    MA orders, respectively.

    if a nonnegative integer, round(x, symbols) will be printed.

    for anything else, print.default(x, ...) will be called.  
  }
  \item{\dots}{
    optional arguments passed to other print methods
  }
}
\details{
  Compute the extended sample acf (ESACF) for the time series stored in
  x, ignoring any time information associated with x, using only
  as.numeric(x).  The matrix of ESACF with the AR order up to ar.max and
  the MA order up to ma.max is stored in the matrix EACFM.  The default
  values for NAR and NMA are 7 and 13 respectively.

  
  The 'print' method prints a coded EACF table with significant values
  denoted by 'X' and nosignificant values by 'O', significance level
  being 0.05. 
}
\value{
  for 'eacf':  an object of class 'eacf', being a numeric matrix of
  dimensions (ar.max+1) x (ma.max+1) and an attribute 'nobs'.

  for 'print.eacf':  return(invisible(...) something:  If symbols is a
  character vector of length 2 and attr(x, 'nobs) exceeds sum(dim(x)),
  a character matrix representation of symbols[1:2] is returned
  invisibly.  Otherwise, 'x' rounded to number of digits = symbols or
  'x' itself is returned. 
}
\author{
  K. S. Chan, Department of Statistics and Actuarial Science, University
  of Iowa, for the original 'eacf' code;  Spencer Graves for the
  documentation.   
}
\source{
  
}
\references{
  Ruey Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley,
  ch. 2, pp. 59-61
}
\seealso{
  \code{\link[stats]{acf}}
}
\examples{
# Tsay, p. 60 
data(m.3m4697)
EACF.3m <- eacf(m.3m4697, 6, 12)
print(EACF.3m, 2)
print(EACF.3m) 
# DOES NOT MATCH TABLE 2.5.
# I don't know why, but I'm not going to worry about it now.
# Question referred to Prof. Tsay 2007.12.29.
}
\keyword{ts}


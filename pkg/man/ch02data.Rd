\name{ch02data}
\docType{data}
\alias{ch02data}
\alias{m.ibm2697}
\alias{m.vw2697}
\alias{q.gnp4791}
\alias{m.ibm3dx2603}
\alias{m.3m4697}
\alias{q.gdp4703}
\alias{d.sp9003lev}
\alias{q.jnj}
\alias{m.decile1510}
\alias{w.gs1n36299}  
\title{ financial time series for Tsay (2005, chapter 2[text]) }
\description{
  Financial time series used in examples in chapter 2.  
}
\usage{
m.ibm2697
m.vw2697
q.gnp4791
m.ibm3dx2603
m.3m4697
q.gdp4703
d.sp9003lev
q.jnj
m.decile1510
w.gs1n36299  
}
\format{
  Objects of class zoo giving simple returns for each trading period 
  (day, week or month) for different periods.  

  \item{m.ibm2697, m.vw2697}{
    Monthly returns for IBM stock and the value weighted index from 1926
    to 1997. 
  }  
  \item{q.gnp4791}{
    Growth rate of U.S. quarterly real gnp, from 1947Q2 to 1991Q1.  
  }
  \item{m.ibm3dx2603}{
    Mnthy returns of IBM stock, the value and equal weighted and
    Standard and Poors indices from 1926 through 2003.  
  }
  \item{m.3m4697}{
    Mnthly simple returns of 3M stock from Feb., 1946 through
    Dec. 2003. 
  }
  \item{q.gdp4703}{
    U.S. quarterly GDP from 1947 through 2003
  }
  \item{d.sp9003lev}{
    Daily values of S&P 500 index from 1990 through 2003.  
  }
  \item{q.jnj}{
    Quarterly earnings of Johnson & Johnson from 1960 through 1980.  
  }
  \item{m.decile1510}{
    Monthly simple returns of Deciles 1, 5, 10
  }
  \item{m.gs3, m.gs5}{
    Monthly 3-yr and 5-yr Treasury constant maturity rates
  }
  \item{w.gs1n36299}{
    Weekly 1-yr & 3-yr interest rates
  }
}
%\details{
%}
\source{
  \url{
    http://faculty.chicagogsb.edu/ruey.tsay/teaching/fts2
  }
}
\references{
  Ruey Tsay (2005)
  Analysis of Financial Time Series, 2nd ed. (Wiley)
}
\keyword{datasets}
\seealso{ \code{\link{ch01data}} }
%\examples{
%}


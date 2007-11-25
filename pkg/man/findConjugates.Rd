\name{findConjugates}
\alias{findConjugates}
\title{ Find complex conjugate pairs }
\description{
  Find all complex conjugate pairs in a vector of complex numbers and
  return one number from each pair.  
}
\usage{
findConjugates(x, complex.eps=1000*.Machine[["double.neg.eps"]]) 
}
\arguments{
  \item{x}{
    a vector of complex numbers 
  }
  \item{complex.eps}{
    a small positive number used to identify complex conjugates.
  }
}
\details{
  1.  Compute normalization m2 = outer(abs(x), abs(x), max) 

  2.  Compute complex differences c2 = abs(outer(x, Conj(x), "-"))/m2 

  3.  If any abs(c2) < complex.eps, make sure the numbers are not
  duplicate reals via (d2 = abs(outer(x, x, "-"))) > complex.eps 
}
\value{
  a complex vector with one representative of each complex pair found
}
\seealso{
  \code{\link{plotArmaTrueacf}}
}
\examples{
# none
findConjugates(NULL)
findConjugates(numeric(0))
findConjugates(0:4)
findConjugates(rep(0:1,each=3))

# some 
findConjugates(c(1+1i, 0, 1-1i, 2-2i, 3, 2+2i, NA))
}
\keyword{arith}

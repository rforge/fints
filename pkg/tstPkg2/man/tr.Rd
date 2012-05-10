\name{tr}
\alias{tr}
\title{
  Matrix transpose
}
\description{
  Function to test t(Matrix()) without import(Matrix), etc.
}
\usage{
tr(x)
}
\arguments{
  \item{x}{
    an object to be transposed.
  }
}
\value{
  t(x)
}
\examples{
M. <- M()
mt <- tr(as.matrix(M.))

# check
\dontshow{stopifnot(}
all.equal(as.matrix(M.), mt)
\dontshow{)}

\dontrun{
# This doesn't work, because R can't find
# the version of t.default in Marix:
Mt <- tr(M.)

### R CMD check ends as follows:
* checking examples ... ERROR
Running examples in 'tstPkg2-Ex.R' failed
The error most likely occurred in:

> ### Name: tr
> ### Title: Matrix transpose
> ### Aliases: tr
> ### Keywords: manip
>
> ### ** Examples
>
> M. <- M()
> Mt <- tr(M.)
Error in t.default(x) : argument is not a matrix
Calls: tr -> t -> t.default
Execution halted
}

}
\keyword{manip}

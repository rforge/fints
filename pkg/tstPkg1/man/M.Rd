\name{M}
\alias{M}
\title{
  Create a test Matrix
}
\description{
  Create a test Matrix to test in a package that does not officially use
  the Matrix package.
}
\usage{
M(row.=1, col.=1)
}
\arguments{
  \item{row., col.}{
    Number of rows and columns in the matrix
  }
}
\value{
  Matrix(1:(row.*col.), row., col.)
}
\examples{
M. <- M()

# check
\dontshow{stopifnot(}
all.equal(M., Matrix(1))
\dontshow{)}

}
\keyword{manip}

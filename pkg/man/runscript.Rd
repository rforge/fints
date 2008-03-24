\name{runscript}
\alias{runscript}
\title{ Run a package script }
\description{
  Run a script associated with a particular chapter 
}
\usage{
runscript(x, fmt="ch\%02d.R", package="FinTS", 
          subdir="scripts", lib.loc=NULL)
}
\arguments{
  \item{x}{
    an object to identify a file in package/subdir via sprintf(fmt, x).

    For example, by default, x = 2 translates to 'ch02.R'.
  }
  \item{fmt}{
    a format to be used with 'x' in \link{sprintf} to create the name of
    a file in lib.loc/package/subdir.  
  }
  \item{subdir}{
    subdirectory of package containing a file of the name constructed
    via sprintf(fmt, x)  
  }
  \item{package}{
    Name of a package with subdirectory 'subdir'.  
  }
  \item{lib.loc}{
    NULL or character string identifying the location where
    'system.file(subdir, package, lib.loc)' will find the folder
    containing the file identified via sprintf(fmt, x)  
  }
}
\details{
  similar to \link{demo}  
}
%\value{
%}
%\references{
%}
\author{
  Gabor Grothendieck 
}
\seealso{
  \code{\link[utils]{demo}}
  \code{\link[base]{sprintf}}
  \code{\link[base]{system.file}}
}
\examples{
\dontrun{
# run ~R\library\FinTS\scripts\ch01.R 
runscript(1)

# run ~R\library\nlme\scripts\afda-ch01.R
if(require(fda)) 
     runscript(1, fmt="afda-ch%02d.R", package="fda") 
}
}
\keyword{utilities}


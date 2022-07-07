\name{perl_flavor}
\alias{perl_flavor}
\alias{perl_flavor<-}
\title{
Retrieve of Set Flavor of Regular Expression
}
\description{
Retrieve or set the \code{"perl"} property of an object
  of the class \code{'re'}.
}
\usage{
perl_flavor(x)

perl_flavor(x) <- value

}
\arguments{
  \item{x}{
   an oject of the class \code{'re'}.
  }
  \item{value}{
   a logical vector of length one. If \code{value} is \code{TRUE},
   then the \code{'perl'} property of \code{x} is set to
   \code{TRUE}. If \code{value} is \code{FALSE},
   then the \code{'perl'} property of \code{x} is set to
   \code{FALSE}.
  } 
}
\value{
Returns a logical value.
}
\details{
The assigment function merely sets the \code{"perl"} property. It does not
  actually modify the regular expression notation. In other words, if you
  set the \code{"perl"} property to the `wrong' value, then the regular
  expression will no longer function properly in any of the functions that
  support \code{'re'} objects.
}
\seealso{
  \code{\link{re}}
}
\examples{
(regex <- re("^.{3,}"))
perl_flavor(regex)

perl_flavor(regex) <- FALSE
perl_flavor(regex)
regex

perl_flavor(regex) <- TRUE
perl_flavor(regex)
regex

}

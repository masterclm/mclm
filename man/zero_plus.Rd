\name{zero_plus}
\alias{zero_plus}
\title{
Make all items in a vector strictly higher than zero
}
\description{
Simple helper function that takes a vector \code{x} and returns a copy of
  \code{x} in which all values less than or equal to zero are replaced with
  the value \code{small_pos}.
}
\usage{
zero_plus(x, small_pos = 0.00001)
}
\arguments{
  \item{x}{
  a vector of numbers.
  }
  \item{small_pos}{
  a small positive number.
  }
}
\value{
Returns a copy of \code{x} in which all values less than or equal to zero
are replaced with the values \code{small_pos}.
}
\examples{
(x <- rnorm(30))
zero_plus(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }

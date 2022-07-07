\name{as_numeric}
\alias{as_numeric}
\title{
Coerce Object to Numeric Vector
}
\description{
The generic method \code{as_numeric} turns its first argument \code{x},
  or at least part of the information in \code{x},
  into a numeric vector.
}
\usage{
as_numeric(x, \dots)
}
\arguments{
  \item{x}{
   any object.
  }
  \item{\dots}{
   additional arguments.
  }
}
\details{
The generic method \code{as_numeric} is nothing but an alternative notation for
the base R generic method \code{as.numeric}.
}
\value{
This function returns a numeric vector.
}
\examples{
(flist <- freqlist(tokenize("The old story of the old man and the sea.")))

# extract frequency counts from a frequency list
as_numeric(flist)
as.numeric(flist)

# preferable alternative
type_freqs(flist)

}


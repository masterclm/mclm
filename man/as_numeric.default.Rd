\name{as_numeric.default}
\alias{as_numeric.default}
\title{
Coerce Object to Numeric Vector
}
\description{
The default S3 method \code{as_numeric.default} turns its first
  argument \code{x}, or at least part of the information in \code{x},
  into a numeric vector.
}
\usage{
\method{as_numeric}{default}(x, \dots)
}
\arguments{
  \item{x}{
   any object.
  }
  \item{\dots}{
   additional arguments.
  }
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


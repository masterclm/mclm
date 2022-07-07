\name{n_types.freqlist}
\alias{n_types.freqlist}
\title{
Give Number of Types in a 'freqlist' Object
}
\description{
The S3 method \code{n_types}, when applied to an object \code{x} of the
  class \code{'freqlist'}, returns the number of types in \code{x}.
}
\usage{
\method{n_types}{freqlist}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'freqlist'}.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{n_types} returns a number.
}
\examples{
(tks <- tokenize("The old man and the sea."))
n_tokens(tks)

(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)
}


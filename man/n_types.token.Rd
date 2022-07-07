\name{n_types.tokens}
\alias{n_types.tokens}
\title{
Give Number of Types in a 'tokens' Object
}
\description{
The S3 method \code{n_types}, when applied to an object \code{x} of the
  class \code{'tokens'}, returns the number of types in \code{x}.
}
\usage{
\method{n_types}{tokens}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'tokens'}.
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
n_tokens(tks) # retrieved directly from 'tokens' object
n_types(tks)  # number of types is calculated on the spot
(tps <- types(tks))
n_tokens(tps) # returns NA
n_types(tps)  # retrieved directly from 'types' object
}


\name{n_types.types}
\alias{n_types.types}
\title{
Give Number of Types in a 'types' Object
}
\description{
The S3 method \code{n_types}, when applied to an object \code{x} of the
  class \code{'types'}, returns the number of types in \code{x}.
}
\usage{
\method{n_types}{types}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'types'}.
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
(tps <- types(tks))
n_types(tps)
}


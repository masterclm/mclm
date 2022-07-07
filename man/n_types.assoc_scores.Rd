\name{n_types.assoc_scores}
\alias{n_types.assoc_scores}
\title{
Give Number of Types in an Association Scores Object
}
\description{
The S3 method \code{n_types}, when applied to an object \code{x} of the
  class \code{'assoc_scores'}, returns the number of types in \code{x}.
}
\usage{
\method{n_types}{assoc_scores}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'assoc_scores'}.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{n_types} returns a number.
}
\examples{
a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")
(scores <- assoc_abcd(a, b, c, d, types = types))
n_types(scores)
type_names(scores)
}


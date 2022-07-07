\name{type_names.assoc_scores}
\alias{type_names.assoc_scores}
\title{
Give Names of Types Represented in an Association Scores List 
}
\description{
The generic method \code{type_names}, when applied to an object \code{x} of the
  class \code{'assoc_scores'}, returns the names of the types
  represented in
  the object \code{x}. 
}
\usage{
\method{type_names}{assoc_scores}(x, \dots)
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
Returns a character vector.
}
\examples{
a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")
(scores <- assoc_abcd(a, b, c, d, types = types))
type_names(scores)
}

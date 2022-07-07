\name{as_character.default}
\alias{as_character.default}
\title{
Coerce Object to Character Vector
}
\description{
The default S3 method \code{as_character.default} turns its first
  argument \code{x}, or at least part of the information in \code{x},
  into a character vector.
}
\usage{
\method{as_character}{default}(x, \dots)
}
\arguments{
  \item{x}{
   any object for which no more specific \code{as_character}-method exists.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
This function returns a character vector.
}
\examples{
as_character(1:10) # turn numeric vector into character vector
as.character(1:10) # alternative approach

}


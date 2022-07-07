\name{as_character.re}
\alias{as_character.re}
\alias{as.character.re}
\title{
Coerce Object of class 're' to Character Vector
}
\description{
The S3 methods \code{as_character} and \code{as.character}, when
  applied to an argument \code{x} of the class \code{"re"}, return
  a character vector containing the regular expression (but not
  containing any information on its \code{"perl"} property).
}
\usage{
\method{as_character}{re}(x, \dots)

\method{as.character}{re}(x, \dots)

}
\arguments{
  \item{x}{
   an object of the class \code{'re'}.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
These methods returns a character vector.
}
\examples{
regex <- re("(?xi) ^ .*")
as_character(regex) # turn 're' object into character vector
as.character(regex) # alternative approach

}


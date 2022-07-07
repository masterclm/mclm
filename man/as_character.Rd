\name{as_character}
\alias{as_character}
\title{
Coerce Object to Character Vector
}
\description{
The generic method \code{as_character} turns its first argument \code{x},
  or at least part of the information in \code{x},
  into a character vector.
}
\usage{
as_character(x, \dots)
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
This function returns a character vector.
}
\examples{
(tks <- tokenize("The old man and the sea."))
as_character(tks) # turn 'tokens' object into character vector
as.character(tks) # alternative approach

}


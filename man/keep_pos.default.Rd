\name{keep_pos.default}
\alias{keep_pos.default}
\alias{drop_pos.default}
\title{
Make a subset of an object on the basis of a posean vector
}
\description{
The function \code{keep_pos} is a generic function that takes as its
  first argument \code{x}
  an object that represents
  a collection of linguistic units, for instance an object of the
  class \code{'freqlist'}, and extracts from it a selection on
  the basis of the posean values in \code{pos}. The posean
  vector \code{pos} must contain as many items as \code{x}.
  
  The function \code{drop_pos} behaves identical to \code{keep_pos}
  being called with \code{invert} set to \code{TRUE}.
  
  Warning: if \code{x} is of a class for which no dedicated
  \code{keep_pos} and
  \code{drop_pos} functions exist, then the dummy functions
  \code{keep_pos.default} and \code{drop_pos.default} are used, which
  don't do any real selecting, but simply return \code{x}.
}
\usage{
\method{keep_pos}{default}(x, pos, invert = FALSE, \dots)

\method{drop_pos}{default}(x, pos, \dots)

}
\arguments{
  \item{x}{
  an object that represents a collection of linguistic units, for instance
  an object of the class \code{'freqlist'}. 
  }
  \item{pos}{
  a posean vector, of the same length as \code{x}. 
  }
 \item{invert}{
  a posean vector of length one, which indicates whether the matches
  or the non-matches should be selected.
  }
 \item{\dots}{
 other arguments.
 }
}
\value{
Returns an object containing information on the selected types only. The
resulting object is
of the same class as \code{x}.
}
\examples{
(tps <- as_types(letters[1:10]))

keep_pos(tps, c(1, 3, 5, 7, 9))
drop_pos(tps, c(1, 3, 5, 7, 9))

}

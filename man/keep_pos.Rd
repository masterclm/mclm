\name{keep_pos}
\alias{keep_pos}
\alias{drop_pos}
\title{
Subset of Object Based on Positions
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
}
\usage{
keep_pos(x, pos, invert = FALSE, \dots)

drop_pos(x, pos, \dots)

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

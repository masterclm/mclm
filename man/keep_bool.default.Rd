\name{keep_bool.default}
\alias{keep_bool.default}
\alias{drop_bool.default}
\title{
Make a subset of an object on the basis of a boolean vector
}
\description{
The function \code{keep_bool} is a generic function that takes as its
  first argument \code{x}
  an object that represents
  a collection of linguistic units, for instance an object of the
  class \code{'freqlist'}, and extracts from it a selection on
  the basis of the boolean values in \code{bool}. The boolean
  vector \code{bool} must contain as many items as \code{x}.
  
  The function \code{drop_bool} behaves identical to \code{keep_bool}
  being called with \code{invert} set to \code{TRUE}.
  
  Warning: if \code{x} is of a class for which no dedicated
  \code{keep_bool} and
  \code{drop_bool} functions exist, then the dummy functions
  \code{keep_bool.default} and \code{drop_bool.default} are used, which
  don't do any real selecting, but simply return \code{x}.
}
\usage{
\method{keep_bool}{default}(x, bool, invert = FALSE, \dots)

\method{drop_bool}{default}(x, bool, \dots)

}
\arguments{
  \item{x}{
  an object that represents a collection of linguistic units, for instance
  an object of the class \code{'freqlist'}. 
  }
  \item{bool}{
  a boolean vector, of the same length as \code{x}. 
  }
 \item{invert}{
  a boolean vector of length one, which indicates whether the matches
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
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

keep_bool(flist, flist < 2)
drop_bool(flist, flist >= 2)
keep_bool(flist, ranks(flist) <= 3)

print(flist, sort_order = "none")
keep_bool(flist, c(FALSE, TRUE, TRUE, FALSE)) 

y <- keep_bool(flist, flist < 2)
y
keep_bool(y, orig_ranks(y) > 2)

keep_bool(y, orig_ranks(y) > 5)

}

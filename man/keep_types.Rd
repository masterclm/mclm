\name{keep_types}
\alias{keep_types}
\alias{drop_types}
\title{
Make a subset of an object on the basis of a selection of types
}
\description{
Generic function that takes as its first argument \code{x}
  an object that represents
  a collection of linguistic units, for instance an object of the
  class \code{'freqlist'}, and extracts from it those units that
  involve one of the types in \code{types}.
  
  The function \code{drop_types} behaves identical to \code{keep_types}
  being used with \code{invert = TRUE}.

}
\usage{
keep_types(x, types, invert = FALSE, \dots)

drop_types(x, types, \dots)
}
\arguments{
  \item{x}{
  an object that represents a collection of linguistic units, for instance
  an object of the class \code{"freqlist"}. 
  }
  \item{types}{
  a character vector, which can be either and object of the class
  \code{'types'} or any other character vector,
  containing the types that will be selected. 
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
keep_types(flist, c("man", "and"))
drop_types(flist, c("man", "and"))
keep_types(flist, c("man", "and"), invert = TRUE) # same as drop_types()

}

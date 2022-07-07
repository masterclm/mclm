\name{freqlist_diff}
\alias{freqlist_diff}
\title{
Subtract frequencies found in second frequency list for first frequency list
}
\description{
Merge information from two frequency lists, subtracting the frequencies found in the 
second frequency list for the frequencies found in the first frequency list

}
\usage{
freqlist_diff(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   the object \code{x} must be a frequency list. 
  }
  \item{y}{
   the object \code{y} must be a frequency list. 
  }
}
\value{
This function returns a frequency list, i.e. an object of the
  class \code{"freqlist"}.
}
\examples{
(flist1 <- freqlist("A first toy toy toy corpus.", as_text = TRUE))
(flist2 <- freqlist("A second toy corpus corpus.", as_text = TRUE))
freqlist_diff(flist1, flist2)

}

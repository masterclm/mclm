\name{freqlist_merge}
\alias{freqlist_merge}
\title{
Merge two frequency lists, adding up the frequencies
}
\description{
Merge two frequency lists into one frequency list, adding up the frequencies
}
\usage{
freqlist_merge(x, y)
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
(flist1 <- freqlist("A first toy corpus.", as_text = TRUE))
(flist2 <- freqlist("A second toy corpus.", as_text = TRUE))
freqlist_merge(flist1, flist2)

}

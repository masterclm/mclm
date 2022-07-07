\name{freqlist_merge_all}
\alias{freqlist_merge_all}
\title{
Merge several frequency lists, adding up the frequencies
}
\description{
Merge several frequency lists into one frequency list, adding up the frequencies
}
\usage{
freqlist_merge_all(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
   all arguments must be either objects of the class 'freqlist' or
   R lists containing such objects 
  }
}
\value{
This function returns a frequency list, i.e. an object of the
  class \code{"freqlist"}.
}
\examples{
(flist1 <- freqlist("A first toy corpus.", as_text = TRUE))
(flist2 <- freqlist("A second toy corpus.", as_text = TRUE))
(flist3 <- freqlist("A third toy corpus.", as_text = TRUE))
freqlist_merge_all(flist1, flist2, flist3)
freqlist_merge_all(list(flist1, flist2, flist3)) # same result


}

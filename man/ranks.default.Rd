\name{ranks.default}
\alias{ranks.default}
\title{
Retrieve the current ranks for frequency counts
}
\description{
Generic function that takes as its argument \code{x}
  an object that represents frequency counts, for instance an object of the
  class \code{'freqlist'}, and retrieves from it the current ranks
  for those frequency counts. Those current ranks may differ from the
  original ranks, for instance, if
  \code{x} contains frequency counts for a selection of items only, and not
  for all tokens in the corpus.
  Frequencies are ranked in descending order. Ties all receive unique ranks,
  with lower ranks being assigned to the ties that are encountered earlier
  in the data.
}
\usage{
\method{ranks}{default}(x, \dots)
}
\arguments{
  \item{x}{
  an object that represents frequency counts, for instance an object of the
  class \code{'freqlist'}. 
  }
  \item{\dots}{
    additional arguments.
  }
}
\value{
Returns a numeric vector, representing the current ranks, with as its names
  the types to which these ranks apply.
}
\examples{
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

orig_ranks(flist)
ranks(flist)

(flist2 <- keep_types(flist, c("man", "and")))

orig_ranks(flist2)
ranks(flist2)

}

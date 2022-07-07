\name{ranks.freqlist}
\alias{ranks.freqlist}
\title{
Retrieve the Ranks of the Items in Frequency Lists
}
\description{
Function that takes as its argument \code{x}
  an object of the
  class \code{'freqlist'}, and retrieves from it the ranks
  of the items in the frequency list. 
  These ranks are integer values running from one up to the number of items
  in \code{x}. Each items receives a unique rank.
  Items are first ranked by frequency in descending order. Items with
  identical frequency are further ranked by alphabetic order.
}
\usage{
\method{ranks}{freqlist}(x, with_names = FALSE, \dots)
}
\arguments{
  \item{x}{
  an object of the
  class \code{'freqlist'}. 
  }
  \item{with_names}{
  a logical value indicating whether or not the items in the output should
  be given names. If \code{with_names} is \code{TRUE}, then the names
  of the types in the frequency list are used as names.
  }
  \item{\dots}{
    additional arguments.
  }
}
\value{
Returns a numeric vector, representing the current ranks, with as its names
  the types to which these ranks apply.
}
\details{
  The \code{mclm} method \code{ranks}, applied to frequency lists, is not
  to be confused with the base R function \code{rank}. There are two
  important differences.
  
  First, the base R function \code{rank} always ranks items from low values to
  high values and the \code{mclm} method \code{ranks} ranks from high
  frequency items to low frequency items.
  
  Second, the base R function \code{rank} allows the user to choose among
  a number of different ways
  to handle ties. The \code{mclm} method \code{ranks} on the other hand
  always handles ties in the same way. 
  More specifically, items with identical frequencies are always
  ranked in alphabetical order.
  
  In other words, the base R function \code{rank} is a flexible tool that
  supports a number of different ranking methods that are commonly used in
  statistics. The \code{mclm} method \code{ranks} on the other hand is a
  rigid tool that supports only one type of ranking, which is a type of
  ranking that is atypical from a statistics point of view, but is
  commonly used in linguistic frequency lists. Also, it is designed to be
  unaffected by the order of the items in the frequency list.
}
\examples{
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

orig_ranks(flist)
ranks(flist)
ranks(flist, with_names = TRUE)

(flist2 <- keep_types(flist, c("man", "and")))

orig_ranks(flist2)
ranks(flist2)

}

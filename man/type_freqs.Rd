\name{type_freqs}
\alias{type_freqs}
\alias{type_freq}
\title{
Retrieve Frequencies from 'freqlist' Object
}
\description{
This function takes as its argument \code{x}
  an object an object of the class \code{'freqlist'}, and retrieves from it
  the frequencies of either alll or some of its items. 
}
\usage{
type_freqs(x, types = NULL, with_names = FALSE, \dots)
type_freq(x, types = NULL, with_names = FALSE, \dots)
}
\arguments{
  \item{x}{
  an object of the class \code{'freqlist'}. 
  }
  \item{types}{
  \code{NULL} or a character vector or an object of the class
  \code{'types'}.
  
  If the argument \code{types} is \code{NULL}, then
  the frequencies of all the items in \code{x} are returned, in the order
  in which these items appear in \code{x}.
  
  If the argument \code{types} is
  a character vector or an object of the class \code{'types'}, then only
  the frequencies (in \code{x}) of the items in \code{'types'} are
  given, in the order in which these items appear in \code{'types'}. For all
  items in \code{'types'} that do not occur in \code{x}, a frequency of
  zero is returned.
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
Returns a numeric vector, representing the frequencies of the items.
}
\examples{
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

type_freqs(flist) # frequencies of all items
type_names(flist) # names of all items

type_freqs(flist, with_names = TRUE) # frequencies of all types, with names
 
type_freqs(flist, c("man", "the")) # frequencies of specific items ...
type_freqs(flist, c("the", "man")) # ... in the requested order
type_freq(flist, "the")            # frequency of one item

# frequencies of specific items can also be printed using subsetting
flist[c("the", "man")] 
flist["the"]           


}

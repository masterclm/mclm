\name{type_names.freqlist}
\alias{type_names.freqlist}
\title{
Give Names of Types Represented in a Frequency List 
}
\description{
The generic method \code{type_names}, when applied to an object \code{x} of the
  class \code{'freqlist'}, returns the names of the types
  represented in
  the object \code{x}. 
}
\usage{
\method{type_names}{freqlist}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'freqlist'}.
  }
  \item{\dots}{
  additional arguments.
  }
}
\value{
Returns a character vector.
}
\examples{
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

n_types(flist)
type_names(flist)
type_freqs(flist)

}

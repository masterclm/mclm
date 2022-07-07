\name{type_names}
\alias{type_names}
\title{
Give Names of Types Represented in an Object 
}
\description{
The generic method \code{type_names} returns the names of the types
  represented in
  the object \code{x}. Implementations of this method exist for a number of
  different classes.
}
\usage{
type_names(x, \dots)
}
\arguments{
  \item{x}{
   an object of any of the classes for which the method \code{type_names}
   is implemented.
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

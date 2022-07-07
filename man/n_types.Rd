\name{n_types}
\alias{n_types}
\title{
Give Number of Types Represented in an Object
}
\description{
The generic method \code{n_types} returns the number of types represented in
  the object \code{x}. Implementations of this method exist for a number of
  different classes.
}
\usage{
n_types(x, \dots)
}
\arguments{
  \item{x}{
   an object of any of the classes for which the method \code{n_types}
   is implemented.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{n_types} returns a number.
}
\examples{
(tks <- tokenize("The old man and the sea."))
n_tokens(tks)

(tps <- types(tks))
n_types(tps)

(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)



}


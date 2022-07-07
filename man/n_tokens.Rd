\name{n_tokens}
\alias{n_tokens}
\title{
Give Number of Tokens
}
\description{
The generic method \code{n_tokens} returns the number of tokens represented in
  the object \code{x}. Implementations of this method exist for a number of
  different classes.
}
\usage{
n_tokens(x, \dots)
}
\arguments{
  \item{x}{
   an object of any of the classes for which the method \code{n_tokens}
   is implemented.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{n_tokens} returns a number.
}
\examples{
(tks <- tokenize("The old man and the sea."))
n_tokens(tks)

(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)

}


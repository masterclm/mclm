\name{n_tokens.freqlist}
\alias{n_tokens.freqlist}
\title{
Give Number of Tokens in a 'freqlist' Object
}
\description{
The S3 method \code{n_tokens}, when applied to an object \code{x} of the
  class \code{'freqlist'}, returns the number of tokens in \code{x}.
}
\usage{
\method{n_tokens}{freqlist}(x, \dots)
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
The method \code{n_tokens} returns a number.
}
\examples{
(tks <- tokenize("The old man and the sea."))
n_tokens(tks)

(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)
}


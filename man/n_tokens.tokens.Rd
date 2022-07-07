\name{n_tokens.tokens}
\alias{n_tokens.tokens}
\title{
Give Number of Tokens in a 'tokens' Object
}
\description{
The S3 method \code{n_tokens}, when applied to an object \code{x} of the
  class \code{'tokens'}, returns the number of tokens in \code{x}.
}
\usage{
\method{n_tokens}{tokens}(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'tokens'}.
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

}


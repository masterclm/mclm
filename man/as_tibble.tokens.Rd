\name{as_tibble.tokens}
\alias{as_tibble.tokens}
\title{
Coerce 'tokens' Object to tibble
}
\description{
The S3 method \code{as_tibble}, when applied to an object \code{x} of the
 class \code{'tokens'}, coerces an object of the class \code{'tokens'} to a 
 tibble
}
\usage{
\method{as_tibble}{tokens}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an object of the class \code{'tokens'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a tibble. In this tibble, the tokens sit in
 a column named \code{token}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."

(toks <- tokenize(toy_corpus))

as_tibble(toks)



}

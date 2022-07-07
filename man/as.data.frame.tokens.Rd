\name{as.data.frame.tokens}
\alias{as.data.frame.tokens}
\title{
Coerce 'tokens' Object to data frame
}
\description{
The S3 method \code{as.data.frame}, when applied to an object \code{x} of the
 class \code{'tokens'}, coerces an object of the class \code{'tokens'} to a 
 data frame
}
\usage{
\method{as.data.frame}{tokens}(x, \dots)
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
This function returns a data frame. In this data frame, the tokens sit in
 a column named \code{token}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."

(toks <- tokenize(toy_corpus))

as.data.frame(toks)



}

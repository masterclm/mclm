\name{as.data.frame.types}
\alias{as.data.frame.types}
\title{
Coerce 'types' Object to data frame
}
\description{
The S3 method \code{as.data.frame}, when applied to an object \code{x} of the
 class \code{'types'}, coerces an object of the class \code{'types'} to a 
 data frame
}
\usage{
\method{as.data.frame}{types}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an object of the class \code{'types'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a data frame. In this data frame, the types sit in
 a column named \code{type}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."

(typs <- types(toy_corpus, as_text = TRUE))

as.data.frame(typs)



}

\name{as_tibble.types}
\alias{as_tibble.types}
\title{
Coerce 'types' Object to tibble
}
\description{
The S3 method \code{as_tibble}, when applied to an object \code{x} of the
 class \code{'types'}, coerces an object of the class \code{'types'} to a 
 tibble
}
\usage{
\method{as_tibble}{types}(x, \dots)
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
This function returns a tibble. In this tibble, the types sit in
 a column named \code{type}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."

(typs <- types(toy_corpus, as_text = TRUE))

as_tibble(typs)



}

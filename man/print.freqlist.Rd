\name{print.freqlist}
\alias{print.freqlist}
\title{
Print a Frequency List
}
\description{
Print objects of the class \code{'freqlist'}. 
}
\usage{
\method{print}{freqlist}(x,
      n = 20,
      from = 1,
      extra = NULL,
      ...)
}
\arguments{
  \item{x}{
  an object of the class \code{'freqlist'}
  }
  \item{n}{
  number of items to print
  }
  \item{from}{
  position of the first item to print
  }
  \item{extra}{
  extra settings
  }
  \item{...}{
  additional printing arguments
  }
}
\value{
Invisibly returns \code{x}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
(flist <- freqlist(toy_corpus, as_text = TRUE))
print(flist)
print(flist, from = 7, n = 10)

}

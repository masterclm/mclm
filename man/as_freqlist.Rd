\name{as_freqlist}
\alias{as_freqlist}
\title{
Coerce table to a frequency list
}
\description{
Coerces a table to a frequency list
}
\usage{
as_freqlist(x, tot_n_tokens = NULL, sort_by_ranks = TRUE)
}
\arguments{
  \item{x}{
   the object \code{x} must be an object of the class \code{'table'}.
   Alternatively, \code{x} can also be a numeric vector, provided that
   its items have names. In that case, this vector will be interpreted
   as a table.
  }
  \item{tot_n_tokens}{
   a numeric vector of length one. The number \code{tot_n_tokens}
   represents the total number of tokens in the corpus from which
   the frequency list is derived. When \code{tot_n_tokens} is
   \code{NULL}, this total number of tokens will be taken to be
   the sum of the frequencies in \code{x}.
  }
  \item{sort_by_ranks}{
   a boolean vector of length one. If \code{sort_by_ranks} is
   \code{TRUE}, the items in the frequency list are sorted by
   frequency rank. If \code{sort_by_ranks} is
   \code{FALSE}, the items in the frequency list, depending on the
   input type, either are sorted alphabetically or are not sorted
   at all. 
  }
  
}
\value{
This function returns an object of the class \code{'freqlist'}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
## make frequency list in a roundabout way
tokens <- tokenize(toy_corpus)
flist <- as_freqlist(table(tokens))
flist
## more direct procedure
freqlist(toy_corpus, as_text = TRUE)

## build frequency list from scratch: example 1
flist <- as_freqlist(c("a" = 12, "toy" = 53, "example" = 20))
flist

## build frequency list from scratch: example 2
flist <- as_freqlist(c("a" = 12, "toy" = 53, "example" = 20),
                     tot_n_tokens = 1300)
flist

}

\name{as_tibble.freqlist}
\alias{as_tibble.freqlist}
\title{
Coerce frequency list to tibble
}
\description{
Coerces a frequency list to a tibble
}
\usage{
\method{as_tibble}{freqlist}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{'freqlist'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a tibble.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
(flist <- freqlist(toy_corpus, as_text = TRUE))
as_tibble(flist)

(flist2 <- keep_re(flist, "^..?$"))
as_tibble(flist2)


}

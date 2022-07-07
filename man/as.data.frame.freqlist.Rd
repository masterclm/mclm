\name{as.data.frame.freqlist}
\alias{as.data.frame.freqlist}
\title{
Coerce frequency list to data frame
}
\description{
Coerces a frequency list to a data frame
}
\usage{
\method{as.data.frame}{freqlist}(x, row.names = NULL, optional = FALSE, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{'freqlist'}.
  }
  \item{row.names}{	
  \code{NULL} or a character vector giving the row names for the data frame.
  Missing values are not allowed.
  }
  \item{optional}{
  See the base R function \code{\link{as.data.frame}} for the meaning of
  this argument.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a data frame.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
(flist <- freqlist(toy_corpus, as_text = TRUE))
as.data.frame(flist)

(flist2 <- keep_re(flist, "^..?$"))
as.data.frame(flist2)


}

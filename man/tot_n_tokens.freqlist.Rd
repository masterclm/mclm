\name{tot_n_tokens.freqlist}
\alias{tot_n_tokens.freqlist}
\alias{tot_n_tokens<-.freqlist}
\title{
Retrieve or set the total number of tokens on which frequency counts are based
}
\description{
Generic function that takes as its argument \code{x}
  an object 
  class \code{'freqlist'}, and retrieves from it, or sets, the total number
  of tokens in
  the corpus on
  which those frequency counts are based. This total number of tokens 
  may be higher
  than the sum of all frequency counts in \code{x}, for instance, if
  \code{x} contains frequency counts for a selection of items only, and not
  for all tokens in the corpus.
}
\usage{
\method{tot_n_tokens}{freqlist}(x)
\method{tot_n_tokens}{freqlist}(x) <- value
}
\arguments{
  \item{x}{
  an object  of the
  class \code{'freqlist'}. 
  }
  \item{value}{
  a numerical value. 
  }
}
\value{
Returns a number.
}
\examples{
x <- freqlist("The man and the mouse.",
              re_token_splitter = "(?xi) [:\\\\s.;,?!\\"]+",
              as_text = TRUE)
x
tot_n_tokens(x)
y <- keep_types(x, c("man", "and"))
tot_n_tokens(y)
y
tot_n_tokens(y) <- sum(y)
y
tot_n_tokens(y)

}

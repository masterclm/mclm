\name{orig_ranks.freqlist}
\alias{orig_ranks.freqlist}
\alias{orig_ranks<-.freqlist}
\title{
Retrieve or set the original ranks for a frequency list
}
\description{
Generic function that takes as its argument \code{x}
  an object of the
  class \code{'freqlist'}, and retrieves from it, or sets, the original ranks
  for its frequency counts.
  
    These original ranks are only defined if \code{x} is the result of
  a selection procedure (i.e. if
  \code{x} contains frequency counts for a selection of items only, and not
  for all tokens in the corpus).
}
\usage{
\method{orig_ranks}{freqlist}(x, with_names = FALSE, \dots)
\method{orig_ranks}{freqlist}(x) <- value
}
\arguments{
  \item{x}{
  an object that represents frequency counts, for instance an object of the
  class \code{'freqlist'}. 
  }
 \item{with_names}{
  a logical value indicating whether or not the items in the output should
  be given names. If \code{with_names} is \code{TRUE}, then the names
  of the types in the frequency list are used as names.
  }  
  \item{value}{
  a value; currently, the only accepted value is \code{NULL} 
  }
  \item{\dots}{
  additional arguments.
  }
}
\value{
Returns either \code{NULL}, or a numeric vector, representing the
 original ranks, with as its names the types to which these ranks apply.
}
\examples{
x <- freqlist("The man and the mouse.",
              as_text = TRUE)
x
orig_ranks(x)
orig_ranks(x, with_names = TRUE)
y <- keep_types(x, c("man", "and"))
orig_ranks(y)
y
orig_ranks(y) <- NULL
y
orig_ranks(y)
tot_n_tokens(y) <- sum(y)
y

}

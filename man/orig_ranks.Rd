\name{orig_ranks}
\alias{orig_ranks}
\alias{orig_ranks<-}
\title{
Retrieve or set the original ranks for frequency counts
}
\description{
Generic function that takes as its argument \code{x}
  an object that represents frequency counts, for instance an object of the
  class \code{'freqlist'}, and retrieves from it, or sets, the original ranks
  for those frequency counts.
  
  These original ranks are only defined if \code{x} is the result of
  a selection procedure (i.e. if
  \code{x} contains frequency counts for a selection of items only, and not
  for all tokens in the corpus).
}
\usage{
orig_ranks(x, \dots)
orig_ranks(x) <- value
}
\arguments{
  \item{x}{
  an object that represents frequency counts, for instance an object of the
  class \code{'freqlist'}. 
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
y <- keep_types(x, c("man", "and"))
orig_ranks(y)
y
orig_ranks(y) <- NULL
y
orig_ranks(y)
tot_n_tokens(y) <- sum(y)
y

}

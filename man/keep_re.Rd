\name{keep_re}
\alias{keep_re}
\alias{drop_re}
\title{
Make a subset of an object on the basis of a regular expression
}
\description{
Generic function that takes as its first argument \code{x}
  an object that represents
  a collection of linguistic units, for instance an object of the
  class \code{'freqlist'}, and extracts from it those units that
  match the regular expression in \code{pattern}.
  
  The function \code{drop_re} behaves identical to \code{keep_re}
  being used with \code{invert = TRUE}.

}
\usage{
keep_re(x, pattern, perl = TRUE, invert = FALSE, \dots)

drop_re(x, pattern, perl = TRUE, \dots)

}
\arguments{
  \item{x}{
  an object that represents a collection of linguistic units, for instance
  an object of the class \code{'freqlist'}. 
  }
  \item{pattern}{
  a character vector of length one, which contains a regular expression.
  }
  \item{perl}{
  a boolean vector of length one, which indicates whether or not
  \code{pattern} is treated as a PCRE flavor regular expression.
  }
  \item{invert}{
  a boolean vector of length one, which indicates whether the matches
  or the non-matches should be selected.
  }
  \item{\dots}{
  additional arguments.
  }
}
\value{
Returns an object containing information on the selected types only. The
resulting object is
of the same class as \code{x}.
}
\examples{
(flist <- freqlist("The man and the mouse.", as_text = TRUE))

keep_re(flist, "[ao]")

drop_re(flist, "[ao]")
keep_re(flist, "[ao]", invert = TRUE) # same as drop_re()

}

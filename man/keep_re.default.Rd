\name{keep_re.default}
\alias{keep_re.default}
\alias{drop_re.default}
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
  
  Warning: if \code{x} is of a class for which no dedicated
  \code{keep_re} and
  \code{drop_re} functions exist, then the dummy functions
  \code{keep_re.default} and \code{drop_re.default} are used, which
  don't do any real selecting, but simply return \code{x}.

}
\usage{
\method{keep_re}{default}(x, pattern, perl = TRUE, invert = FALSE, \dots)

\method{drop_re}{default}(x, pattern, perl = TRUE, \dots)

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
keep_re(flist, "[ao]", invert = FALSE) # same as drop_re()


}

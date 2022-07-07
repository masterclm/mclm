\name{trunc_at}
\alias{trunc_at}
\title{
Truncates sequence of character data at position matching pattern
}
\description{
Generic function that takes as its argument \code{x}
  an object that represents a sequence of character data, for instance an object of the
  class \code{'tokens'}, and truncates it at the position where a 
  match for the argument \code{pattern} is found.
}
\usage{
trunc_at(x, pattern, \dots)
}
\arguments{
  \item{x}{
  an object that represents a sequence of character data, for instance an object of the
  class \code{'tokens'}. 
  }
  \item{pattern}{
  a regular expression. 
  }
  \item{\dots}{
  additional arguments. 
  }
}
\value{
Returns a truncated version of \code{x}.
}
\examples{
(toks <- tokenize('This is a first sentence . This is a second sentence .',
                  re_token_splitter = '\\\\s+'))

trunc_at(toks, re("[.]"))

}

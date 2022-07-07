\name{sort.tokens}
\alias{sort.tokens}
\title{
Sort a Sequence of Tokens
}
\description{
Sort an object of the class \code{'tokens'}.
}
\usage{

\method{sort}{tokens}(x,
     decreasing = FALSE,
     \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'tokens'}.
  }
  \item{decreasing}{
   logical value that indicates whether the items should be
   sorted from small to large (when \code{decreasing} in
   \code{FALSE}) or from large to small
   (when \code{decreasing} is \code{TRUE}). The default
   value is \code{FALSE}.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The function described on this page returns an object of the class
  \code{'tokens'}.
}
\details{
  At the moment, tokens sequences are not allowed to contain
  \code{NA} values. Therefore, no function argument is available
  that specifies how \code{NA} values should be sorted.
}
\examples{

(tks <- tokenize("the old man and the sea."))
sort(tks)
sort(tks, decreasing = TRUE)

#sort(flist)

}

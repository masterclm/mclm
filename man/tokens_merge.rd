\name{tokens_merge}
\alias{tokens_merge}
\alias{tokens_merge_all}
\title{
Merge 'tokens' Objects
}
\description{
  The function \code{tokens_merge} merges two 'tokens' objects \code{x}
  and \code{y} into one larger 'tokens' object. The function
  \code{tokens_merge_all} merges all the arguments in \code{\dots} into
  one 'tokens' object. The result of a \emph{merge} operation, when applied
  to 'tokens' objects, contains the concatenation of the merged objects.
  In other words, the order of the items in the input is preserved in the
  output.
}
\usage{
tokens_merge(x, y)

tokens_merge_all(\dots)

}
\arguments{
  \item{x}{
   an object of the class \code{'tokens'}.
  }
  \item{y}{
   an object of the class \code{'tokens'}.
  }
  \item{\dots}{
   one or more arguments, which are either objects of the class
   \code{'tokens'} or lists containing such objects.
  }
}
\value{
  The functions \code{tokens_merge} and \code{tokens_merge_all} return
  an object of the class \code{'tokens'}.
}
\examples{
(tks1 <- tokenize(c("This is a first sentence.")))
(tks2 <- tokenize(c("It is followed by a second one.")))
(tks3 <- tokenize(c("Then a third one follows.")))

tokens_merge(tks1, tks2) 
tokens_merge_all(tks1, tks2, tks3)
tokens_merge_all(list(tks1, tks2, tks3))
}

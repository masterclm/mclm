\name{types_merge}
\alias{types_merge}
\alias{types_merge_all}
\title{
Merge 'types' Objects
}
\description{
  The function \code{types_merge} merges two 'types' objects \code{x}
  and \code{y} into one larger 'types' object. The function
  \code{types_merge_all} merges all the arguments in \code{\dots} into
  one 'types' object. The result of a \emph{merge} operation, when applied
  to 'types' objects, contains a concatenation of the merged objects from which
  duplicate items are removed (keeping only the first occurrence). In other
  words, duplicates are removed, but apart from that, the order of the items
  in the input is preserved in the output (unless the argument \code{sort} is set to
  \code{TRUE}).
}
\usage{
types_merge(x, y, sort = FALSE)

types_merge_all(\dots, sort = FALSE)

}
\arguments{
  \item{x}{
   an object of the class \code{'types'}.
  }
  \item{y}{
   an object of the class \code{'types'}.
  }
  \item{\dots}{
   one or more arguments, which are either objects of the class
   \code{'tokens'} or lists containing such objects.
  }
  \item{sort}{
   boolean value that indicates whether the result should be sorted.
  }
}
\value{
  The functions \code{types_merge} and \code{types_merge_all} return
  an object of the class \code{'types'}.
}
\examples{
(tps1 <- as_types(c("a", "simple", "simple", "example")))
(tps2 <- as_types(c("with", "a", "few", "words")))
(tps3 <- as_types(c("just", "for", "testing")))
         
types_merge(tps1, tps2)       # always removes duplicates, but doesn't sort
sort(types_merge(tps1, tps2)) # same, but with sorting

types_merge_all(tps1, tps2, tps3)
types_merge_all(list(tps1, tps2, tps3))
}

\name{fnames_merge}
\alias{fnames_merge}
\alias{fnames_merge_all}
\title{
Merge 'fnames' Objects
}
\description{
  The function \code{fnames_merge} merges two 'fnames' objects \code{x}
  and \code{y} into one larger 'fnames' object. The function
  \code{fnames_merge_all} merges all the arguments in \code{\dots} into
  one 'fnames' object. The result of a \emph{merge} operation, when applied
  to 'fnames' objects, contains a concatenation of the merged objects from which
  duplicate items are removed (keeping only the first occurrence). In other
  words, duplicates are removed, but apart from that, the order of the items
  in the input is preserved in the output (unless the argument \code{sort}
  is set to \code{TRUE}).
}
\usage{
fnames_merge(x, y, sort = FALSE)

fnames_merge_all(\dots, sort = FALSE)

}
\arguments{
  \item{x}{
   an object of the class \code{'fnames'}.
  }
  \item{y}{
   an object of the class \code{'fnames'}.
  }
  \item{\dots}{
   one or more arguments, which are either objects of the class
   \code{'fnames'} or lists containing such objects.
  }
  \item{sort}{
   boolean value that indicates whether the result should be sorted.
  }
}
\value{
  The functions \code{fnames_merge} and \code{fnames_merge_all} return
  an object of the class \code{'fnames'}.
}


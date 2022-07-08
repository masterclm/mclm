% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/types.R
\name{types_merge}
\alias{types_merge}
\alias{types_merge_all}
\title{Merge 'types' objects}
\usage{
types_merge(x, y, sort = FALSE)

types_merge_all(..., sort = FALSE)
}
\arguments{
\item{x, y}{An object of class \code{types}.}

\item{sort}{Boolean value that indicates whether the result should be sorted.}

\item{...}{Either objects of the class \code{types} or lists containing such objects.}
}
\value{
An object of the class \code{types}.
}
\description{
Merge \code{types} objects:
  \code{types_merge} merges two objects, whereas \code{types_merge} merges all
  the objects provided to it. Duplicates are removed, but apart from that,
  the order of the items in the input is preserved in the output
  (unless the argument \code{sort} is set to \code{TRUE}).
}
\section{Functions}{
\itemize{
\item \code{types_merge}: Merge two types

\item \code{types_merge_all}: Merge multiple types
}}

\examples{
(tps1 <- as_types(c("a", "simple", "simple", "example")))
(tps2 <- as_types(c("with", "a", "few", "words")))
(tps3 <- as_types(c("just", "for", "testing")))
types_merge(tps1, tps2)       # always removes duplicates, but doesn't sort
sort(types_merge(tps1, tps2)) # same, but with sorting
types_merge_all(tps1, tps2, tps3)
types_merge_all(list(tps1, tps2, tps3))
}

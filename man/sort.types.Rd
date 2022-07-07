\name{sort.types}
\alias{sort.types}
\title{
Sort a Collection of Types
}
\description{
Sort an object of the class \code{'types'}.
}
\usage{

\method{sort}{types}(x,
     decreasing = FALSE,
     \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'types'}.
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
  \code{'types'}.
}
\details{
  At the moment, types collections are not allowed to contain
  \code{NA} values. Therefore, no function argument is available
  that specifies how \code{NA} values should be sorted.
}
\examples{

(tps <- as_types(c("the", "a", "some", "no")))
sort(tps)
sort(tps, decreasing = TRUE)

}

\name{sort.fnames}
\alias{sort.fnames}
\title{
Sort a Collection of Filenames
}
\description{
Sort an object of the class \code{'fnames'}.
}
\usage{

\method{sort}{fnames}(x,
     decreasing = FALSE,
     \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'fnames'}.
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
  \code{'fnames'}.
}
\details{
  At the moment, filename collections are not allowed to contain
  \code{NA} values. Therefore, no function argument is available
  that specifies how \code{NA} values should be sorted.
}

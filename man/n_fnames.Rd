\name{n_fnames}
\alias{n_fnames}
\title{
Give Number of Items in a Filenames Collection
}
\description{
  Counts the number of items in an \code{'fnames'} object.
}
\usage{
n_fnames(x, \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'fnames'}
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{n_fnames} returns a number.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames()
n_fnames(cwd_fnames)
}

}


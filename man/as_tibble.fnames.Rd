\name{as_tibble.fnames}
\alias{as_tibble.fnames}
\title{
Coerce 'fnames' Object to tibble
}
\description{
The S3 method \code{as_tibble}, when applied to an object \code{x} of the
 class \code{'fnames'}, coerces an object of the class \code{'fnames'} to a 
 tibble
}
\usage{
\method{as_tibble}{fnames}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an object of the class \code{'fnames'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a tibble. In this tibble, the filenames sit in
 a column named \code{filename}.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
as_tibble(cwd_fnames)
}
}

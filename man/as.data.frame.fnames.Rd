\name{as.data.frame.fnames}
\alias{as.data.frame.fnames}
\title{
Coerce 'fnames' Object to data frame
}
\description{
The S3 method \code{as.data.frame}, when applied to an object \code{x} of the
 class \code{'fnames'}, coerces an object of the class \code{'fnames'} to a 
 data frame
}
\usage{
\method{as.data.frame}{fnames}(x, \dots)
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
This function returns a data frame. In this data frame, the filenames sit in
 a column named \code{filename}.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
as.data.frame(cwd_fnames)
}
}

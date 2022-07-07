\name{summary.fnames}
\alias{summary.fnames}
\alias{print.summary.fnames}
\title{
Succinct Description of a 'fnames' Object
}
\description{
Build and/or print an object of the class \code{'summary.fnames'}.
}
\usage{

\method{summary}{fnames}(object, \dots)
\method{print}{summary.fnames}(x, \dots)
 
}
\arguments{
  \item{object}{
   an object of the class \code{'fnames'}.
  }
  \item{x}{
   an object of the class \code{'summary.fnames'}, which is the
   type of object that
   is returned by the \code{summary} method described on this page.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The methods described on this page return an object of the class
  \code{'summary.fnames'}.
}

\examples{

\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
summary(cwd_fnames)
}

}

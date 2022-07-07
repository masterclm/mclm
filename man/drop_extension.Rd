\name{drop_extension}
\alias{drop_extension}
\title{
Removes Filename Extension from Filenames
}
\description{
Simple helper function that takes an \code{'fnames'} object, i.e. a
 collection of filenames, and returns a copy of
 that object with the filename extensions removed.
}
\usage{
drop_extension(x, \dots)
}
\arguments{
  \item{x}{
  an \code{'fnames'} object.
  }
  \item{\dots}{
  additional arguments.
  }
}
\value{
Returns an \code{'fnames'} object.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames()
drop_path(cwd_fnames)
drop_extension(cwd_fnames)
short_names(cwd_fnames) # same as drop_path(drop_extension(cwd_fnames))
}
}

\name{write_fnames}
\alias{write_fnames}
\title{
Writes a collection of filenames to a text file
}
\description{
  Writes an object of the class \code{'fnames'} to a text file. Each type
  is written to a separate line. The file encoding that is used is
  \code{"UTF-8"}.
}
\usage{
write_fnames(x,
             file,
             make_config_file = TRUE,
             ...) 
}
\arguments{
  \item{x}{
   an object of the class \code{'fnames'}.
  }
  \item{file}{
   name of the output file.
  }
  \item{make_config_file}{
   boolean value indicating whether or not next to the actual 'fnames' file,
   a second output file should be created containing a brief description of
   the format of the 'fnames' file.
  }
  \item{...}{
   not used in the current implementation of \code{write_fnames}.
  }
}
\value{
  The function \code{write_fnames} invisibly returns \code{x}.
}
\seealso{
See also \code{\link{read_fnames}}.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
write_fnames(cwd_fnames, "file_with_filenames.txt")
cwd_fnames_2 <- read_fnames("file_with_filenames.txt")
}
}

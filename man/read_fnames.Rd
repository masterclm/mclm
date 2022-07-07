\name{read_fnames}
\alias{read_fnames}
\title{
Read a collection of filenames from a text file
}
\description{
  Reads an object of the class \code{'fnames'} from a text file. By default,
  the text file is assumed to contain one filename on each line.
}
\usage{
read_fnames(file,
            sep = NA,
            file_encoding = "UTF-8",
            trim_fnames = FALSE,
            ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{sep}{
   If not \code{is.ba(sep)}, then \code{sep} must be a character vector
   of length one. In that case, \code{sep} is interpreted as a
   filename separator in the input file. This separator then serves as an
   additional filename separator, next to the end of each line.
   The end of a line always indicated a separator between filenames (in other
   words, filenames cannot cross lines).
  }
  \item{file_encoding}{
   the file encoding used in the input file.
  }
  \item{trim_fnames}{
   boolean value that indicates whether or not leading and trailing
   whitespace should be stripped from the filenames.
  }
  \item{...}{
   not used in the current implementation of \code{read_fnames}.
  }
}
\value{
  The function \code{read_fnames} returns an object of the class
  \code{'fnames'}.
}
\seealso{
See also \code{\link{write_fnames}}.
}
\examples{
\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
write_fnames(cwd_fnames, "file_with_filenames.txt")
cwd_fnames_2 <- read_fnames("file_with_filenames.txt")
}
}

\name{read_types}
\alias{read_types}
\title{
Read a vector of types from a text file
}
\description{
  Reads an object of the class \code{'types'} from a text file. By default,
  the text file is assumed to contain one type on each line.
}
\usage{
read_types(file,
           sep = NA,
           file_encoding = "UTF-8",
           trim_types = FALSE,
           remove_duplicates = FALSE,
           sort = FALSE,
           ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{sep}{
   If not \code{is.ba(sep)}, then \code{sep} must be a character vector
   of length one. In that case, \code{sep} is interpreted as a
   type separator in the input file. This separator the serves as an
   additional type separator, next to the end of each line.
   The end of a line always indicated a separator between types (in other
   words, types cannot cross lines).
  }
  \item{file_encoding}{
   the file encoding used in the input file.
  }
  \item{trim_types}{
   boolean value that indicates whether or not leading and trailing
   whitespace should be stripped from the types.
  }
  \item{remove_duplicates}{
   length one boolean vector that determines whether or not
   duplicates are removed from \code{x} prior to coercing
   to a vector of types
  }
  \item{sort}{
   length one boolean vector that determines whether or not
   \code{x} is alphabetically sorted prior to coercing
   to a vector of types; this argument is ignored if
   \code{remove_duplicates} is \code{TRUE}, because the
   result of removing duplicates is always sorted.
  }
  \item{...}{
   not used in the current implementation of \code{read_types}.
  }
}
\value{
  The function \code{read_types} returns an object of the class
  \code{'types'}.
}
\seealso{
See also \code{\link{write_types}}.
}
\examples{
\dontrun{
types <- as_types(c("first", "second", "third"))
write_types(types, "file_with_types.txt")
types_2 <- read_types("file_with_types.txt")
}
}

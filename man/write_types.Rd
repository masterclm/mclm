\name{write_types}
\alias{write_types}
\title{
Writes a vector of types to a text file
}
\description{
  Writes an object of the class \code{'types'} to a text file. Each type
  is written to a separate line. The file encoding that is used is
  \code{"UTF-8"}.
}
\usage{
write_types(x,
            file,
            make_config_file = TRUE,
            ...) 
}
\arguments{
  \item{x}{
   an object of the class \code{'types'}.
  }
  \item{file}{
   name of the output file.
  }
  \item{make_config_file}{
   boolean value indicating whether or not next to the actual types file,
   a second output file should be created containing a brief description of
   the format of the types file.
  }
  \item{...}{
   not used in the current implementation of \code{write_types}.
  }
}
\value{
  The function \code{write_types} invisibly returns \code{x}.
}
\seealso{
See also \code{\link{read_types}}.
}
\examples{
\dontrun{
types <- as_types(c("first", "second", "third"))
print(types, n = 1000)
write_types(types, "file_with_types.txt")
types_2 <- read_types("file_with_types.txt")
print(types_2, n = 1000)
}
}

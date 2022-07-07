\name{write_tokens}
\alias{write_tokens}
\title{
Writes a 'tokens' Object to a Text File
}
\description{
  Writes an object of the class \code{'tokens'} to a text file. Each token
  is written to a separate line. The file encoding that is used is
  \code{"UTF-8"}.
}
\usage{
write_tokens(x,
             file,
             make_config_file = FALSE,
             ...) 
}
\arguments{
  \item{x}{
   an object of the class \code{'tokens'}.
  }
  \item{file}{
   name of the output file.
  }
  \item{make_config_file}{
   logical value indicating whether or not next to the actual tokens file,
   a second output file should be created containing a brief description of
   the format of the tokens file.
  }
  \item{...}{
   additional arguments.
  }
}
\value{
  The function \code{write_tokens} invisibly returns \code{x}.
}
\seealso{
See also \code{\link{read_tokens}}.
}
\examples{
\dontrun{
(tks <- tokenize("The old man and the sea."))
write_tokens(tks, "file_with_tokens.txt")
(tks2 <- read_tokens("file_with_tokens.txt"))
}
}

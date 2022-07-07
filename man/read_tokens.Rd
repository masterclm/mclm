\name{read_tokens}
\alias{read_tokens}
\title{
Read a 'tokens' Object from a Text File
}
\description{
  Reads an object of the class \code{'tokens'} from a text file. The
  text file is assumed to contain one token on each line. The text file
  is not assumed to have a header (i.e. there is no first line with
  column names).
}
\usage{
read_tokens(file,
            file_encoding = "UTF-8",
            ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{file_encoding}{
   the file encoding used in the input file.
  }
  \item{...}{
   additional arguments.
  }
}
\value{
  The function \code{read_tokens} returns an object of the class
  \code{'tokens'}.
}
\seealso{
\code{\link{write_tokens}}.
}
\examples{
\dontrun{
(tks <- tokenize("The old man and the sea."))
write_tokens(tks, "file_with_tokens.txt")
(tks2 <- read_tokens("file_with_tokens.txt"))
}
}

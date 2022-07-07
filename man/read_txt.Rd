\name{read_txt}
\alias{read_txt}
\title{
Read a text file into a character vector
}
\description{
  Reads a text file and returns a character vector containing the
  lines in the text file (in such a way that the
  lines in the text file are the items in the character vector).
}
\usage{
read_txt(file,
         file_encoding = "UTF-8",
         line_glue = NA,
         ...)
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{file_encoding}{
   the encoding used in the input file.
  }
  \item{line_glue}{
   if \code{line_glue} is \code{NA}, then the output is a character vector
   in which
   each input line is a separate item. If \code{line_glue} is a character
   vector, e.g. \code{"\\n"},
   then the output is a character vector of length one, in which all input
   lines are
   concatenated, using the value of \code{line_glue[1]} as line separator
   and as end-of-last-line marker.
  }
  \item{...}{
   not used in the current implementation of \code{read_txt}.
  }
}
\value{
The function \code{read_txt} returns a character vector.
}
\seealso{
See also \code{\link{write_txt}}.
}
\examples{

# create a small text
x <- "This is
a small
text."

# write the text to a text file
write_txt(x, "example-text-file.txt")

# read a text from file
y <- read_txt("example-text-file.txt")
y

}
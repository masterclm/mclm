\name{write_txt}
\alias{write_txt}
\title{
Write a character vector to a text file
}
\description{
  Writes a character vector to a text file, in such a way that the
  items in the character vector become lines in the text file.
}
\usage{
write_txt(x,
          file = "",
          file_encoding = "UTF-8",
          line_glue = "\n")
}
\arguments{
  \item{x}{
   the character vector that is to be written to file
  }
  \item{file}{
   name of the input file
  }
  \item{file_encoding}{
   the encoding that is to be used in the output file.
  }
  \item{line_glue}{
   if \code{line_glue} is a character string, then this
   character string is used as end-of-line marker on disk.
   if \code{line_glue} is \code{NA}, no end-of-line
   markers are used.
  }
}
\value{
  The function \code{write_txt} (invisibly) returns the value of its
  first argument \code{x}.
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

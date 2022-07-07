\name{read_conc}
\alias{read_conc}
\title{
Read a concordance from file
}
\description{
  Reads concordance-based data frames that are written to file with the
  function  \code{\link{write_conc}}. 
}
\usage{
read_conc(file,
          header = TRUE,
          sep = "\t",
          quote = "",
          comment_char = "",
          file_encoding = "UTF-8",
          stringsAsFactors = default.stringsAsFactors(),
          ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{header}{
   boolean value that expresses whether or not the first line in the
   input file contains column names. In the current implementation of
   \code{read_conc}, the value of \code{header} is ignored and
   is assumed to be \code{TRUE}.
  }
  \item{sep}{
   the field separator that is used in the input file.
  }
  \item{quote}{
   boolean value that expresses whether or not character data fields in the
   file are enclosed in quotation marks. This argument is   
   not used in the current implementation of \code{read_conc}. For the
   moment, character data are assumed not to be enclosed in quotes.
  }
  \item{comment_char}{
   not used in the current implementation of \code{read_conc}. For the
   moment the comment character mechanism is disabled.
  }
  \item{file_encoding}{
   the encoding used in the input file.
  }
  \item{stringsAsFactors}{
   boolean value: should character data automatically be converted to
   factors? This argument applies to all columns in the data, with the
   exception of the columns \code{source}, \code{left}, \code{match}, and
   \code{right}, which are never converted to factors.
  }
  \item{...}{
   not used in the current implementation of \code{read_conc}.
  }
}
\value{
  The function \code{read_conc} returns a data frame of the class
  \code{conc}.
}
\seealso{
See also \code{\link{write_conc}}.
}
\examples{
(d <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
write_conc(d, "example_data.tab")
(d2 <- read_conc("example_data.tab"))

}

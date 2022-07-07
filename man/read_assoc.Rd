\name{read_assoc}
\alias{read_assoc}
\title{
Read association scores from file
}
\description{
  Reads association scores that are written to file with the
  function  \code{\link{write_assoc}}. 
}
\usage{
read_assoc(file,
           header = TRUE,
           sep = "\t",
           quote = "",
           comment_char = "",
           file_encoding = "UTF-8",
           ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{header}{
   boolean value that expresses whether or not the first line in the
   input file contains column names. In the current implementation of
   \code{read_assoc}, the value of \code{header} is ignored and
   is assumed to be \code{TRUE}.
  }
  \item{sep}{
   the field separator that is used in the input file.
  }
  \item{quote}{
   boolean value that expresses whether or not character data fields in the
   file are enclosed in quotation marks. This argument is   
   not used in the current implementation of \code{read_assoc}. For the
   moment, character data are assumed not to be enclosed in quotes.
  }
  \item{comment_char}{
   not used in the current implementation of \code{read_assoc}. For the
   moment the comment character mechanism is disabled.
  }
  \item{file_encoding}{
   the encoding used in the input file.
  }
  \item{...}{
   not used in the current implementation of \code{read_assoc}.
  }
}
\value{
  The function \code{read_assoc} returns a data frame of the class
  \code{assoc_scores}.
}
\seealso{
See also \code{\link{write_assoc}}.
}
\examples{
txt1 <- "we're just two lost souls swimming in a fish bowl,
         year after year, running over the same old ground,
         what have we found? the same old fears.
         wish you were here."
flist1 <- freqlist(txt1, as_text = TRUE)
txt2 <- "picture yourself in a boat on a river
         with tangerine dreams and marmelade skies
         somebody calls you, you answer quite slowly
         a girl with kaleidoscope eyes"
flist2 <- freqlist(txt2, as_text = TRUE)
(scores <- assoc_scores(flist1, flist2, min_freq = 0))
write_assoc(scores, "example_scores.tab")
(scores2 <- read_assoc("example_scores.tab"))

}

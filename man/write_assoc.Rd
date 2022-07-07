\name{write_assoc}
\alias{write_assoc}
\title{
Write association scores to file.
}
\description{
  Writes an object of the class \code{assoc_scores} to file.
}
\usage{
write_assoc(x,
            file = "",
            sep = "\\t",
            file_encoding = "UTF-8")
}
\arguments{
  \item{x}{
   the association scores that are to be written to file.
  }
  \item{file}{
   name of the output file.
  }
  \item{sep}{
   the field separator that is to be used in the output file.
  }
  \item{file_encoding}{
   the encoding that is to be used in the output file.
  }
}
\value{
  The function \code{write_assoc} (invisibly) returns the value of
  its first argument, viz. \code{x}.
}
\seealso{
See also \code{\link{read_assoc}}.
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
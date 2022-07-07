\name{write_conc}
\alias{write_conc}
\title{
Write a concordance to file.
}
\description{
  Writes an object of the class \code{conc} to file.
}
\usage{
write_conc(x,
           file = "",
           sep = "\\t",
           file_encoding = "UTF-8")
}
\arguments{
  \item{x}{
   the concordance that is to be written to file.
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
  The function \code{write_conc} (invisibly) returns the value of
  its first argument, viz. \code{x}.
}
\seealso{
See also \code{\link{read_conc}}.
}
\examples{
cd <- conc('A very small corpus.', '\\\\w+', as_text = TRUE)
write_conc(cd, "example_data.csv")
(cd_2 <- read_conc("example_data.csv"))
as.data.frame(cd_2)

}
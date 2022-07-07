\name{import_conc}
\alias{import_conc}
\title{
Imports a concordance from files generated with another tool
}
\description{
  Imports a concordance-based data frames from files that were created
  with another means than the \code{mclm} function \code{\link{write_conc}}. 
}
\usage{
import_conc(x,
            file_encoding = "UTF-8",
            source_type = c("corpuseye"),
            ...) 
}
\arguments{
  \item{x}{
   a vector of input filenames
  }
  \item{file_encoding}{
   file encoding of the files referred to in \code{x}.
  }
  \item{source_type}{
   string that identifies the tool the field separator that is used in the input file.
  }
  \item{...}{
   not used in the current implementation of \code{import_conc}.
  }
}
\value{
  The function \code{import_conc} returns a data frame of the class
  \code{conc}.
}
\seealso{
See also \code{\link{read_conc}}, \code{\link{write_conc}}.
}
\examples{
(cd <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
write_conc(cd, "example_data.csv")
(cd_2 <- read_conc("example_data.csv"))

}

\name{write_dataset}
\alias{write_dataset}
\title{
Write a linguistic dataset (of the type used in \code{mclm}) to file
}
\description{
  Writes to file datasets of the type that are read with 
  \code{\link{read_dataset}}, etc. In principle, it should
  also be possible to write such datasets with the function 
  \code{\link{write.table}}, but in some locales
  the function \code{write_dataset}
  can prove to be slightly more robust in case the files contain
  Non-Western language data.
}
\usage{
write_dataset(x,
              file = "",
              sep = "\\t",
              file_encoding = "UTF-8")
}
\arguments{
  \item{x}{
   the dataset that is to be written to file
  }
  \item{file}{
   name of the output file
  }
  \item{sep}{
   the field separator that is to be used in the output file.
  }
  \item{file_encoding}{
   the encoding that is to be used in the output file.
  }
}
\value{
  The function \code{write_dataset} (invisibly) returns the value of
  its first argument, viz. \code{x}.
}
\seealso{
See also \code{\link{read_dataset}}.
}
\examples{
\dontrun{
d <- conc('A very small corpus.', '\\\\w+', as_text = TRUE)
write_dataset(d, "example_data.tab")
d2 <- read_dataset("example_data.tab")
d2
}
}
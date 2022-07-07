\name{read_dataset}
\alias{read_dataset}
\title{
Read a linguistic dataset (of the type used in \code{mclm})
}
\description{
  Reads datasets of the type that are written to file with the
  function  \code{\link{write_dataset}}. In principle, it should
  also be possible to read such datasets with the function 
  \code{\link{read.table}}, but in some locales the function
  \code{read_dataset}
  can prove to be slightly more robust in case the files contain
  Non-Western language data.
}
\usage{
read_dataset(file,
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
   \code{read_dataset}, the value of \code{header} is ignored and
   is assumed to be \code{TRUE}.
  }
  \item{sep}{
   the field separator that is used in the input file.
  }
  \item{quote}{
   boolean value that expresses whether or not character data fields in the
   file are enclosed in quotation marks. This argument is   
   not used in the current implementation of \code{read_dataset}. For the
   moment, character data are assumed not to be enclosed in quotes.
  }
  \item{comment_char}{
   not used in the current implementation of \code{read_dataset}. For the
   moment the comment character mechanism is disabled.
  }
  \item{file_encoding}{
   the encoding used in the input file.
  }
  \item{stringsAsFactors}{
   boolean value: should character data automatically be converted to
   factors?
  }
  \item{...}{
   not used in the current implementation of \code{read_dataset}.
  }
}
\value{
  The function \code{read_dataset} returns a data frame.
}
\seealso{
See also \code{\link{write_dataset}}.
}
\examples{
\dontrun{
d <- conc('A very small corpus.', '\\\\w+', as_text = TRUE)
write_dataset(d, "example_data.tab")
d2 <- read_dataset("example_data.tab")
d2
}
}

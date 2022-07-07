\name{read_freqlist}
\alias{read_freqlist}
\title{
Read a frequency list from a csv file
}
\description{
  Reads an object of the class \code{'freqlist'} from a csv file. The csv
  file is assumed to contain two columns, the first being the type and the
  second being the frequency of that type. The file is also assumed to
  have a header line with the names of both columns. 
}
\usage{
read_freqlist(file,
              sep = "\\t",
              file_encoding = "UTF-8",
              ...) 
}
\arguments{
  \item{file}{
   name of the input file
  }
  \item{sep}{
   Length one character vector with the column separator in the input
   file.
  }
  \item{file_encoding}{
   the file encoding used in the input file.
  }
  \item{...}{
   not used in the current implementation of \code{read_freqlist}.
  }
}
\details{
  The function \code{read_freqlist} not only reads the file \code{file},
  but also chevks whether a configuration file exists with a name that
  is identical to \code{file}, except that it has the filename extension
  \code{".yaml"}. If such a file exists, then that configuration file
  is taken to 'belong' to \code{file} and is also read. If such a
  configuration file exists, then the frequency list attributes
  \code{"tot_n_tokens"} and \code{"tot_n_types"} are retrieved from
  that configuration file. If no such configuration file exists,
  then the values for \code{"tot_n_tokens"} and \code{"tot_n_types"} are
  calculated on the basis of the frequencies in the frequency list. 
}
\value{
  The function \code{read_freqlist} returns an object of the class
  \code{'freqlist'}.
}
\seealso{
See also \code{\link{write_freqlist}}.
}
\examples{
\dontrun{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
freqs <- freqlist(toy_corpus, as_text = TRUE)
print(freqs, n = 1000)
write_freqlist(freqs, "example_freqlist.csv")
freqs2 <- read_freqlist("example_freqlist.csv")
print(freqs2, n = 1000)
}
}

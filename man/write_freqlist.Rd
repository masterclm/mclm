\name{write_freqlist}
\alias{write_freqlist}
\title{
Write a frequency list to a csv file
}
\description{
  Writes an object of the class \code{'freqlist'} to a csv file. The
  resulting csv
  file contains two columns, the first being the type and the
  second being the frequency of that type. The file also contains
  a header line with the names of both columns. 
}
\usage{
write_freqlist(x,
               file,
               sep = "\\t",
               make_config_file = TRUE,
               ...) 
}
\arguments{
  \item{x}{
    an object of the class \code{'freqlist'}.
  }
  \item{file}{
   a character vector of length one that represents the name of the
   output file.
  }
  \item{sep}{
   a character vector of length one that represents the column separator in
   the output file.
  }
  \item{make_config_file}{
   a boolean vector of length one that determines whether or not a
   configuration file needs to be created. In most circumstances, this
   should be set to \code{TRUE}.
   file.
  }
  \item{...}{
   not used in the current implementation of \code{write_freqlist}.
  }
}
\details{
  The function \code{write_freqlist} not only writes to the file \code{file},
  but also creates a configuration file  with a name that
  is identical to \code{file}, except that it has the filename extension
  \code{".yaml"}. The frequency list attributes
  \code{"tot_n_tokens"} and \code{"tot_n_types"} are stored to that
  configuration file. 
}
\value{
  The function \code{write_freqlist} invisibly returns 
  \code{x}.
}
\seealso{
See also \code{\link{read_freqlist}}.
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

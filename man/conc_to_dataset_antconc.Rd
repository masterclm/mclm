\name{conc_to_dataset_antconc}
\alias{conc_to_dataset_antconc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert concordances generated with AntConc to a dataset
}
\description{
  Reads one or several concordances generated with AntConc, and
  converts their contents to a single dataset that can be read
  with the function \code{\link{read_dataset}}.
  The encoding in the output is
   'UTF-8'.
}
\usage{
conc_to_dataset_antconc(x,
                        outfile,
                        version = c("3.4.3", "3.2.4"),
                        file_encoding = "")
}
\arguments{
  \item{x}{
   name of the input file(s). This can be either a single name, or
   a longer vector with several names. In the latter case, the
   contents of all input files is merged before writing the
   output file.
  }
  \item{outfile}{
   Name of the output file. The encoding in the output is
   'UTF-8'.
  }
  \item{version}{
   Version of AntConc that was used to create the \code{infiles}.
   You can only specify one version (that is then assumed to apply
   to all files in \code{x}).
  }
  \item{file_encoding}{
   file encoding in the input file(s). If all input files have the
   same encoding, then assign a single character string to
   \code{fileEncoding}. If different input files have different
   encodings, then assign to \code{file_encoding} a character data
   vector of the same length as \code{x}.
  }
}
\value{
  This function invisibly returns the value of its first argument
  \code{x}.
}

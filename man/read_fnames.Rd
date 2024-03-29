% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnames.R
\name{read_fnames}
\alias{read_fnames}
\title{Read a collection of filenames from a text file}
\usage{
read_fnames(file, sep = NA, file_encoding = "UTF-8", trim_fnames = FALSE, ...)
}
\arguments{
\item{file}{Path to input file.}

\item{sep}{Character vector of length 1 or \code{NA}. If it is a character, it
indicates a separator between input files, in addition to the new line.}

\item{file_encoding}{Encoding used in the input file.}

\item{trim_fnames}{Boolean. Should leading and trailing whitespace be stripped
from the filenames?}

\item{...}{Additional arguments (not implemented).}
}
\value{
An object of class \code{\link{fnames}}.
}
\description{
This function reads an object of class \code{\link{fnames}} from a text file, which is
assumed to contain one filename on each line.
}
\examples{
\dontshow{.old_wd <- setwd(tempdir())}
cwd_fnames <- as_fnames(c("file1.txt", "file2.txt"))
write_fnames(cwd_fnames, "file_with_filenames.txt")
cwd_fnames_2 <- read_fnames("file_with_filenames.txt")
\dontshow{setwd(.old_wd)}
}
\seealso{
\code{\link[=write_fnames]{write_fnames()}}

Other reading functions: 
\code{\link{read_assoc}()},
\code{\link{read_conc}()},
\code{\link{read_freqlist}()},
\code{\link{read_tokens}()},
\code{\link{read_txt}()},
\code{\link{read_types}()}
}
\concept{reading functions}

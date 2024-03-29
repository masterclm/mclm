% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{read_tokens}
\alias{read_tokens}
\title{Read a \code{tokens} object from a text file}
\usage{
read_tokens(file, file_encoding = "UTF-8", ...)
}
\arguments{
\item{file}{Name of the input file.}

\item{file_encoding}{Encoding to read the input file.}

\item{...}{Additional arguments (not implemented).}
}
\value{
An object of class \code{\link{tokens}}.
}
\description{
This function reads an object of the class \code{\link{tokens}} from a text file, typically
stored with \code{\link[=write_tokens]{write_tokens()}}. The text file is assumed to contain one token on
each line and not to have a header.
}
\examples{
\dontshow{.old_wd <- setwd(tempdir())}
(tks <- tokenize("The old man and the sea."))
write_tokens(tks, "file_with_tokens.txt")
(tks2 <- read_tokens("file_with_tokens.txt"))
\dontshow{setwd(.old_wd)}
}
\seealso{
\code{\link[=write_tokens]{write_tokens()}}

Other reading functions: 
\code{\link{read_assoc}()},
\code{\link{read_conc}()},
\code{\link{read_fnames}()},
\code{\link{read_freqlist}()},
\code{\link{read_txt}()},
\code{\link{read_types}()}
}
\concept{reading functions}

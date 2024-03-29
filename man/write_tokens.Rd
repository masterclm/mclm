% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{write_tokens}
\alias{write_tokens}
\title{Write a \code{tokens} object to a text file}
\usage{
write_tokens(x, file, ...)
}
\arguments{
\item{x}{An object of class \code{\link{tokens}}.}

\item{file}{Name of the output file.}

\item{...}{Additional arguments (not implemented).}
}
\value{
Invisibly, \code{x}.
}
\description{
This function writes an object of the class \code{\link{tokens}} to a text file. Each
token is written to a separate line. The file encoding is always "UTF-8".
This file can later be read with \code{\link[=read_tokens]{read_tokens()}}.
}
\examples{
\dontshow{.old_wd <- setwd(tempdir())}
(tks <- tokenize("The old man and the sea."))
write_tokens(tks, "file_with_tokens.txt")
(tks2 <- read_tokens("file_with_tokens.txt"))
\dontshow{setwd(.old_wd)}
}
\seealso{
\code{\link[=read_tokens]{read_tokens()}}

Other writing functions: 
\code{\link{write_assoc}()},
\code{\link{write_conc}()},
\code{\link{write_fnames}()},
\code{\link{write_freqlist}()},
\code{\link{write_txt}()},
\code{\link{write_types}()}
}
\concept{writing functions}

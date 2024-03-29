% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/freqlist.R, R/tokenize.R
\name{n_tokens}
\alias{n_tokens}
\alias{n_tokens.freqlist}
\alias{n_tokens.tokens}
\title{Count tokens}
\usage{
n_tokens(x, ...)

\method{n_tokens}{freqlist}(x, ...)

\method{n_tokens}{tokens}(x, ...)
}
\arguments{
\item{x}{An object of any of the classes for which the method is implemented.}

\item{...}{Additional arguments.}
}
\value{
A number.
}
\description{
This method returns the number of tokens in an object.
}
\examples{
(tks <- tokenize("The old man and the sea."))
n_tokens(tks)

(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)
}
\seealso{
Other getters and setters: 
\code{\link{n_types}()},
\code{\link{orig_ranks}()},
\code{\link{ranks}()},
\code{\link{tot_n_tokens}()},
\code{\link{type_names}()}
}
\concept{getters and setters}

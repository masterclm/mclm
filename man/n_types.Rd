% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/assoc.R, R/freqlist.R,
%   R/tokenize.R, R/types.R
\name{n_types}
\alias{n_types}
\alias{n_types.assoc_scores}
\alias{n_types.freqlist}
\alias{n_types.tokens}
\alias{n_types.types}
\title{Count types}
\usage{
n_types(x, ...)

\method{n_types}{assoc_scores}(x, ...)

\method{n_types}{freqlist}(x, ...)

\method{n_types}{tokens}(x, ...)

\method{n_types}{types}(x, ...)
}
\arguments{
\item{x}{An object of any of the classes for which the method is implemented.}

\item{...}{Additional arguments.}
}
\value{
A number.
}
\description{
This method returns the number of types in an object.
}
\examples{
(tks <- tokenize("The old man and the sea."))

# for a types object ----------
(tps <- types(tks))
n_types(tps)

# for a freqlist object -------
(flist <- freqlist(tks))
n_tokens(flist)
n_types(flist)

# for an assoc_scores object --
a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")

(scores <- assoc_abcd(a, b, c, d, types = types))
n_types(scores)
}
\seealso{
Other getters and setters: 
\code{\link{n_tokens}()},
\code{\link{orig_ranks}()},
\code{\link{ranks}()},
\code{\link{tot_n_tokens}()},
\code{\link{type_names}()}
}
\concept{getters and setters}

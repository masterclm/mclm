% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/fnames.R, R/freqlist.R,
%   R/tokenize.R, R/types.R
\name{keep_types}
\alias{keep_types}
\alias{drop_types}
\alias{drop_types.fnames}
\alias{keep_types.fnames}
\alias{drop_types.freqlist}
\alias{keep_types.freqlist}
\alias{drop_types.tokens}
\alias{keep_types.tokens}
\alias{drop_types.types}
\alias{keep_types.types}
\title{Subset an object based on a selection of types}
\usage{
keep_types(x, types, invert = FALSE, ...)

drop_types(x, types, ...)

\method{drop_types}{fnames}(x, types, ...)

\method{keep_types}{fnames}(x, types, invert = FALSE, ...)

\method{drop_types}{freqlist}(x, types, ...)

\method{keep_types}{freqlist}(x, types, invert = FALSE, ...)

\method{drop_types}{tokens}(x, types, ...)

\method{keep_types}{tokens}(x, types, invert = FALSE, ...)

\method{drop_types}{types}(x, types, ...)

\method{keep_types}{types}(x, types, invert = FALSE, ...)
}
\arguments{
\item{x}{An object of any of the classes for which the method is implemented.}

\item{types}{Either an object of the class \code{\link{types}}
or a character vector.}

\item{invert}{Logical. Whether the matches should be selected rather than the
non-matches.}

\item{...}{Additional arguments.}
}
\value{
Object of the same class as \code{x} with the selected elements only.
}
\description{
These methods can be used to subset objects based on a list of types.
}
\details{
The methods \code{\link[=keep_pos]{keep_pos()}} and \code{\link[=drop_pos]{drop_pos()}} are part of a family of methods of
the mclm package used to subset different objects. The methods
starting with \code{keep_} extract the items in \code{x} based on the criterion specified
by the second argument. In contrast, the methods starting with \code{drop_} \emph{exclude}
the items that match the criterion in the same argument.

Calling a \code{drop_} method is equivalent to calling its \code{keep_} counterpart when
the \code{invert} argument is \code{TRUE}.
}
\examples{
# For a 'freqlist' object ------------------------
(flist <- freqlist("The man and the mouse.", as_text = TRUE))
keep_types(flist, c("man", "and"))
drop_types(flist, c("man", "and"))
keep_types(flist, c("man", "and"), invert = TRUE) # same as drop_types()

# For a 'types' object ---------------------------
(tps <- as_types(letters[1:10]))

keep_types(tps, c("a", "c", "e", "g", "i"))
drop_types(tps,  c("a", "c", "e", "g", "i"))

# For a 'tokens' object --------------------------
(tks <- as_tokens(letters[1:10]))

keep_types(tks, c("a", "c", "e", "g", "i"))
drop_types(tks,  c("a", "c", "e", "g", "i"))
}
\seealso{
Other subsetters: 
\code{\link{brackets}},
\code{\link{keep_bool}()},
\code{\link{keep_pos}()},
\code{\link{keep_re}()}
}
\concept{subsetters}

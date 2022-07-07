\name{cleanup_spaces}
\alias{cleanup_spaces}
\title{
Clean up the use of whitespace in a character vector
}
\description{
The function \code{cleanup_spaces} takes as its input a character vector
 \code{x}, and returns a copy of \code{x} in which the use of whitespace
 characters has been `normalized'. More specifically, this
 `normalization' entails that each uninterrrupted stretch of whitespace
 characters,
 i.e. each sequence of one or more consecutive whitespace characters,
 is replaced by a single SPACE character. Moreover, in case of the argument
 specification
 \code{remove_leading = TRUE}, all leading whitespace is removed, and
 in case of the argument specification 
 \code{remove_trailing = TRUE}, all trailing whitespace is removed.
}
\usage{
cleanup_spaces(x,
               remove_leading = TRUE,
               remove_trailing = TRUE)
}
\arguments{
  \item{x}{
  the argument \code{x} contains the character vector to which the cleaning
  up has to be applied.
}
  \item{remove_leading}{
  in case of the setting \code{remove_leading = TRUE}, all leading whitespace is
  completely removed; in case of the setting \code{remove_leading = FALSE},
  any leading whitespace is
  `normalized', as explained above, but not completely removed.
}
  \item{remove_trailing}{
  in case of the setting \code{remove_trailing = TRUE}, all trailing whitespace
  is
  completely removed; in case of the setting \code{remove_trailing = FALSE},
  any trailing whitespace is
  `normalized', as explained above, but not completely removed.
}
}
\value{
Returns a character vector that is a copy of \code{x} in which all whitespace is
 `normalized'.
}
\examples{
txt <- " A \\t small   example \\n with redundant whitespace   "
cleanup_spaces(txt)
cleanup_spaces(txt, remove_leading = FALSE, remove_trailing = FALSE)

}

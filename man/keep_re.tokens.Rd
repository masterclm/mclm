\name{keep_re.tokens}
\alias{keep_re.tokens}
\alias{drop_re.tokens}
\alias{keep_pos.tokens}
\alias{drop_pos.tokens}
\alias{keep_bool.tokens}
\alias{drop_bool.tokens}
\alias{keep_types.tokens}
\alias{drop_types.tokens}
\alias{[.tokens}
\alias{[<-.tokens}
\title{
Subset of 'tokens' Object
}
\description{
The S3 methods \code{keep_re()}, \code{keep_pos()}, \code{keep_bool()}, and
  \code{keep_types()}, when applied to an object of the
  class \code{'tokens'}, take as their first argument \code{x}
  an object of the class \code{'tokens'}, and extract from it those
  items that \emph{match} the selection criterion which is their second
  argument, viz. \code{pattern} in \code{keep_re()},
  \code{pos} in \code{keep_pos()},
  \code{bool} in \code{keep_bool()}, and
  \code{types} in \code{keep_types()}. In this documentation, the
  aforementioned methods are collectively called the \code{keep}-methods.
  
  The S3 methods \code{drop_re()}, \code{drop_pos()}, \code{drop_bool()}, and
  \code{drop_types()}, collectively called the \code{drop}-methods,
  behave identical to how
  the \code{keep}-methods work when the argument \code{invert}
  (which by default is \code{FALSE})
  is set to \code{TRUE}. In that case,  the
  items that \emph{do not match} the selection criterion are selected.

  Subset selection for \code{'tokens'} object with the notation \code{[]},
  in which case argument \code{i} is the selection criterion, behaves
  similarly to
  the \code{keep}-methods. Additionally, unlike the
  \code{keep}-methods (and unlike
  the \code{drop}-methods), the \code{[]} notation also supports subset
  assignment.
  For more details on the relation between the \code{[]} notation and
  the \code{keep}-methods,
  see the description of the argument \code{i} below and also see the
  details section.
}
\usage{
\method{keep_re}{tokens}(x, pattern, perl = TRUE, invert = FALSE, \dots)

\method{drop_re}{tokens}(x, pattern, perl = TRUE, \dots)

\method{keep_pos}{tokens}(x, pos, invert = FALSE, \dots)

\method{drop_pos}{tokens}(x, pos, \dots)

\method{keep_bool}{tokens}(x, bool, invert = FALSE, \dots)

\method{drop_bool}{tokens}(x, bool, \dots)

\method{keep_types}{tokens}(x, types, invert = FALSE, \dots)

\method{drop_types}{tokens}(x, types, \dots)

\method{[}{tokens}(x, i, invert = FALSE, \dots)

\method{[}{tokens}(x, i, invert = FALSE, \dots) <- value

}
\arguments{
  \item{x}{
  an object  of the class \code{'tokens'}. 
  }
  \item{pattern}{
  either an object of the class \code{'re'} (see \code{\link{re}})
  or a character vector of
  length one, which contains a regular expression.
  }
  \item{pos}{
  a numeric vector, the numbers in which identify positions (= indices)
  of items in \code{x}. If the numbers are positive, then their values
  point to the items that are to be selected. If the numbers are negative,
  then their absolute values point to the items that are not to be selected.
  Positive and negative numbers must not be mixed.
  }
  \item{bool}{
  a logical vector of the same length as \code{x}. If \code{bool} is not
  of the correct length, it is \emph{recycled}. Assuming \code{invert} is
  \code{FALSE}, those items are selected for which \code{bool} is
  \code{TRUE}.
  }
  \item{types}{
  either an object of the class \code{'types'}
  (see \code{\link{types}} and \code{\link{as_types}})
  or a character vector. Assuming \code{invert} is
  \code{FALSE}, those items are selected the name of which is included
  in \code{types}.
  }
  \item{i}{
  argument \code{i} is the selection criterion that is used in subsetting
  an object of class \code{'tokens'} with the notation
  \code{[]}.
  Depending on the class of the argument \code{i}, subsetting with \code{[]}
  behaves differently:
    \itemize{
    \item if \code{i} is an object of the class \code{'re'}, then subset
          identification is similar to that of \code{keep_re()}.
    \item if \code{i} is a numeric  vector, then subset
          identification is similar to that of \code{keep_pos()}.
    \item if \code{i} is a logical vector, then subset
          identification is similar to that of \code{keep_bool()}.
    \item if \code{i} either is an object of the class \code{'types'} or
          a character vector, then subset
          identification is similar to that of \code{keep_types()}.          
    }
  }
  \item{perl}{
  a boolean vector of length one, which indicates whether or not
  \code{pattern} is treated as a PCRE flavor regular expression.
  The \code{perl} argument is only used if \code{pattern} is a
  regular character vector.
  If
  \code{pattern} is an object of the class \code{'re'}, then the
  \code{perl} argument is ignored, and the relevant information in the
  \code{'re'} object \code{pattern}, viz. the value of \code{pattern$perl}, is
  used instead.
  }
  \item{invert}{
  a boolean vector of length one, which indicates whether the matches
  or the non-matches should be selected.
  }
  \item{\dots}{
  additional arguments.
  }
  \item{value}{
  a character vector containing the new value that is to be assigned to
  the selected subset.
  }
}
\value{
Subsetting returns an object of the class \code{'tokens'} containing 
the selected tokens only. Subset assignment invisibly returns the value
that is assigned to the selected subset.
}
\details{
When the notation \code{x[i, \dots]} is used, it is also possible to
  use the \code{invert} argument (which then is one of the additional
  arguments in \code{\dots}). This \code{invert} argument
  then serves the same purpose
  as the \code{invert} argument in the \code{keep}-methods.
  When the notation \code{x[i, \dots]} is used, and no \code{invert} argument
  is given, then \code{invert} is taken to be \code{FALSE}.
}
\examples{

(tks <- as_tokens(letters[1:10]))

keep_re(tks, "[acegi]")
drop_re(tks, "[acegi]")

keep_pos(tks, c(1, 3, 5, 7, 9))
drop_pos(tks, c(1, 3, 5, 7, 9))

keep_bool(tks, c(TRUE, FALSE))
drop_bool(tks, c(TRUE, FALSE))

keep_types(tks, c("a", "c", "e", "g", "i"))
drop_types(tks,  c("a", "c", "e", "g", "i"))

tks[re("[acegi]")]
tks[c(1, 3, 5, 7, 9)]
tks[c(TRUE, FALSE)]
tks[c("a", "c", "e", "g", "i")]

tks[re("[acegi]"), invert = TRUE]
tks[c(1, 3, 5, 7, 9), invert = TRUE]
tks[c(TRUE, FALSE), invert = TRUE]
tks[c("a", "c", "e", "g", "i"), invert = TRUE]

}

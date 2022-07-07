\name{print.assoc_scores}
\alias{print.assoc_scores}
\title{
Print association scores for a number of linguistic items
}
\description{
Print objects of the class \code{'assoc_scores'}. 
}
\usage{
\method{print}{assoc_scores}(x,
      n = 20,
      from = 1,
      freeze_cols = NULL, 
      keep_cols = NULL,
      drop_cols = NULL,
      from_col = 1,
      sort_order = c("none", "G_signed", "PMI", "alpha"),
      extra = NULL,
      ...)
}
\arguments{
  \item{x}{
  an object of the class \code{'assoc_scores'}
  }
  \item{n}{
  number of items to print
  }
  \item{from}{
  position of the first item to print
  }
  \item{freeze_cols}{
  names of columns that are not affected by the argument \code{from_col}.
  Frozen columns are always printed before non-frozen columns, even if in the
  original order of columns they had another order.
  }
  \item{keep_cols}{
  if \code{keep_cols} is \code{NULL} (and moreover
  \code{drop_cols} is also \code{NULL}), then all the columns
  in \code{x} are printed (or at least, as many of those as
  fit on the screen); alternatively,
  \code{keep_cols} can also contain a character vector with
  column names. In that
  case only the columns with those names are printed.
  }
  \item{drop_cols}{
  if \code{drop_cols} is \code{NULL} (and moreover
  \code{keep_cols} is also \code{NULL}), then all the columns
  in \code{x} are printed (or at least, as many of those as
  fit on the screen); alternatively,
  \code{drop_cols} can also contain a character vector with
  column names. In that
  case (given also that \code{keep_cols} is \code{NULL}) only the
  columns not listed in \code{drop_cols} are printed.
  
  The argument \code{keep_cols} takes precedence over the argument
  \code{drop_cols}; the argument \code{drop_cols} is only taken into
  account if \code{keep_cols} is \code{NULL}. 
  }
  \item{from_col}{
  name or position of the first selected column that is to
  be printed to the right of the frozen columns.
  If \code{from_col} contains a position, then this position
  is a position among all selected columns, excluding the frozen
  columns.
  }
  \item{sort_order}{
  order in which the items are to be printed. Special values
  are \code{"alpha"}
  (meaning that the items are to be sorted alphabetically), and
  \code{"none"} (meaning that the items are not to be sorted).
  Other possible values are the names of any of the columns in \code{x}.
  If \code{sort_order} contains an illegal value, then the value
  \code{"none"} is assumed.

  It is possible to sort by columns that are blocked from printing
  by means of \code{drop_cols} or \code{keep_cols}.
  }
  \item{extra}{
  additional settings
  }
  \item{...}{
  additional printing arguments
  }
}
\value{
Invisibly returns \code{x}.
}
\details{
  The types, i.e. the names, of the linguistic item, although printed in
  the output as if they are a separate column, actually are the row names
  of \code{x}. They are always printed, and cannot be suppressed
  with \code{keep_cols} or \code{drop_cols}.
}
\examples{
a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")
(scores <- assoc_abcd(a, b, c, d, types = types))
print(scores, sort_order = "PMI")
print(scores, sort_order = "alpha")
print(scores, sort_order = "none")
print(scores, sort_order = "nonsense")

print(scores, sort_order = "PMI",
      keep_cols = c("a", "exp_a", "PMI", "G_signed"))
print(scores, sort_order = "PMI",
      keep_cols = c("a", "b", "c", "d", "exp_a", "G_signed"))
print(scores, sort_order = "PMI",
      drop_cols = c("a", "b", "c", "d", "exp_a", "G_signed",
                    "RR_rows", "chi2_signed", "t"))

}

\name{explore.assoc_scores}
\alias{explore.assoc_scores}
\title{
Interactively Navigate Through Association Scores List
}
\description{
The S3 method \code{explore()}, which only works in an interactive R
  session, can be applied to association scores lists. In that case,
  the method takes
  as its first argument an R object \code{x} of the
  class \code{"assoc_scores"}, and it
  opens `exploration mode', in which the user can navigate
  through \code{x} by means of brief commands. Inside `exploration mode',
  the user can ask for the
  list of available commands by keying in \code{?}, followed by ENTER.
  The user can quit `exploration mode' by keying in \code{q}, followed
  by ENTER.
}
\usage{
\method{explore}{assoc_scores}(x,
   n = 20,
   from = 1,
   from_col = 1,
   perl = TRUE,
   sort_order   = c("none", "G_signed", "PMI", "alpha"),
   use_clear = TRUE,
   \dots)

}
\arguments{
  \item{x}{
   an object of the classes \code{"assoc_scores"}.
  }
  \item{n}{
    maximum number of items in the association scores
    list to be shown at once.   
  }
  \item{from}{
    first item in the association scores list that is shown at
    the beginning of
    the ineractive session.
  }
  \item{from_col}{
    index of first column to be displayed (among all selected columns,
    excluding the frozen columns).
  }
  \item{perl}{
    boolean value that indicates whether or not regular expressions used
    in the exploration session use the PERL flavor of regular expression.
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
  \item{use_clear}{
    boolean value. If \code{use_clear} is \code{TRUE}, and if moreover the
    feature is supported by the R environment, the console will be cleared
    in between all interactive steps in the exploration session.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{explore} invisibly returns \code{x}.
}

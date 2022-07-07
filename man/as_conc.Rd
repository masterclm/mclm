\name{as_conc}
\alias{as_conc}
\title{
Coerce data frame to a concordance object
}
\description{
Coerces a data frame to an object of the class \code{conc}.
}
\usage{
as_conc(x,
        left = NA,
        match = NA,
        right = NA,
        keep_original = FALSE,
        ...) 
}
\arguments{
  \item{x}{
   a data frame.
  }
  \item{left}{
   the name of the column in \code{x} that contains the left co-text of
   the concordance. Is \code{is.na(left)}, then this column is assumed
   to have the name \code{"left"}.
  }
  \item{match}{
   the name of the column in \code{x} that contains the match of
   the concordance. Is \code{is.na(match)}, then this column is assumed
   to have the name \code{"match"}.
  }
  \item{right}{
   the name of the column in \code{x} that contains the right co-text of
   the concordance. Is \code{is.na(right)}, then this column is assumed
   to have the name \code{"right"}.
  }
  \item{keep_original}{
  if any of the arguments \code{left}, \code{match}, or \code{right} has
  a non-\code{NA} value different from resp. \code{"left"}, \code{"match"},
  or \code{"right"}, and moreover \code{keep_original} is \code{FALSE},
  then columns with those names are not kept in the output of
  \code{as_conc}.
  }
  \item{...}{
   any additional arguments.
  }
}
\value{
This function returns an object of the class \code{"conc"}.
}
\examples{
(conc_data <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
df <- as.data.frame(conc_data)
as_conc(df)

}


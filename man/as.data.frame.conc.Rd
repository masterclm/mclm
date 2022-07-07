\name{as.data.frame.conc}
\alias{as.data.frame.conc}
\title{
Coerce concordance to data frame
}
\description{
Coerces an object of the class \code{conc} to a data frame
}
\usage{
\method{as.data.frame}{conc}(x, ...)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{"conc"}.
  }
  \item{...}{
   the argument \code{...} is not used at the moment.
  }
}
\value{
This function returns a data frame.
}
\examples{
(conc_data <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
as.data.frame(conc_data)

}

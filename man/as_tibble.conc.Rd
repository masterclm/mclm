\name{as_tibble.conc}
\alias{as_tibble.conc}
\title{
Coerce concordance to tibble
}
\description{
Coerces an object of the class \code{conc} to a tibble
}
\usage{
\method{as_tibble}{conc}(x, ...)
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
This function returns a tibble.
}
\examples{
(conc_data <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
as_tibble(conc_data)

}

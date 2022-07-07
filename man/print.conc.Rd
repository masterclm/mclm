\name{print.conc}
\alias{print.conc}
\title{
Print a concordance-based data frame.
}
\description{
Print objects of the class \code{"conc"}. 
}
\usage{
\method{print}{conc}(x,
      n = 30, 
      ...)
}
\arguments{
  \item{x}{
  a concordance.
  }
  \item{n}{
  number of items to print in a KWIC display.
  }
  \item{...}{
  additional printing arguments
  }
}
\value{
Invisibly returns \code{x}.
}
\examples{
(conc_data <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
print_kwic(conc_data)
print(conc_data, n = 3)
}


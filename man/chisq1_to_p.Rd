\name{chisq1_to_p}
\alias{chisq1_to_p}
\title{
Proportion of chi-squared distribution with 1 df that sits to the right of x
}
\description{
Helper function that takes as its argument a numerical value \code{x} and
 that returns the proportion \var{p} of the chi-squared
 distribution with one degree of freedom that sits to the right of the
 value \code{x}.

}
\usage{
chisq1_to_p(x)
}
\arguments{
  \item{x}{
  a numerical value.
  }
}
\value{
Returns the proportion \var{p} of the chi-squared
 distribution with one degree of freedom that sits to the right of the
 value \code{x}.
}
\examples{
chisq1_to_p(3.84)
}
\keyword{ internal }

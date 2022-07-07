\name{p_to_chisq1}
\alias{p_to_chisq1}
\title{
P right quantile in chi-squared distribution with 1 df
}
\description{
Helper function that takes as its argument a probability \code{p} and
 that returns the \code{p} \emph{right quantile} in the chi-squared
 distribution with one degree of freedom. In other words, it returns
 a value \var{q} that is such that a proportion \code{p} of the
 chi-squared distribution with 1 df lies above \var{q}. 
}
\usage{
p_to_chisq1(p)
}
\arguments{
  \item{p}{
  a proportion.
  }
}
\value{
Returns the \code{p} \emph{right quantile} in the chi-squared
 distribution with one degree of freedom.
}
\examples{
p_to_chisq1(0.05)
}
\keyword{ internal }

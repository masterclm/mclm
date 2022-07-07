\name{summary.types}
\alias{summary.types}
\alias{print.summary.types}
\title{
Succinct Description of a 'types' Object
}
\description{
Build and/or print an object of the class \code{'summary.types'}.
}
\usage{

\method{summary}{types}(object, \dots)
\method{print}{summary.types}(x, \dots)
 
}
\arguments{
  \item{object}{
   an object of the class \code{'types'}.
  }
  \item{x}{
   an object of the class \code{'summary.types'}, which is the
   type of object that
   is returned by the \code{summary} method described on this page.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The methods described on this page return an object of the class
  \code{'summary.types'}.
}

\examples{

tps <- as_types(c("the", "a", "it", "is", "was", "are", "be", "being"))
summary(tps) 
print(summary(tps))

(tps_sum <- summary(tps))
names(tps_sum)
tps_sum[["n_types"]]
tps_sum$n_types

}

\name{as.data.frame.assoc_scores}
\alias{as.data.frame.assoc_scores}
\title{
Coerce association scores to data frame
}
\description{
Coerces association scores to a data frame
}
\usage{
\method{as.data.frame}{assoc_scores}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{'assoc_scores'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a data frame.
}
\examples{
a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")
(scores <- assoc_abcd(a, b, c, d, types = types))

as.data.frame(scores)

}

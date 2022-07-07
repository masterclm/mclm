\name{drop_empty_rc}
\alias{drop_empty_rc}
\title{
Remove the all-zero rows and columns from a matrix
}
\description{
The function \code{drop_empty_rc} takes as its argument a matrix
 \code{x}, which is assumed to contain frequency counts,
 and
 returns a copy of \code{x} from which all rows and columns
 that contain nothing but zeroes are removed.
}
\usage{
drop_empty_rc(x) 

}
\arguments{
  \item{x}{
  a matrix that is assumed to contain frequency counts.
  }
}
\value{
Returns a matrix that is a copy of \code{x} with the all-zero rows and
  columns removed.
}
\details{
  This function is nothing but a simple convenience function.
  \code{drop_empty_rc(x)} is identical to,
  and is implemented as, 
   \code{x[rowSums(x) > 0, colSums(x) > 0, drop = FALSE]}
}
\examples{
## first example
m <- matrix(nrow = 3, byrow = TRUE,
            dimnames = list(c('r1','r2','r3'),
                            c('c1','c2','c3')),
            c(10, 0, 4,
               0, 0, 0,
               5, 0, 7))
m
m2 <- drop_empty_rc(m)
m2

## second example
m <- matrix(nrow = 3, byrow = TRUE,
            dimnames = list(c('r1','r2','r3'),
                            c('c1','c2','c3')),
            c( 0, 0, 4,
               0, 0, 0,
               0, 0, 7))
m
m2 <- drop_empty_rc(m)
m2

## third example
m <- matrix(nrow = 3, byrow = TRUE,
            dimnames = list(c('r1','r2','r3'),
                            c('c1','c2','c3')),
            c( 0, 0, 0,
               0, 0, 0,
               0, 0, 0))
m
m2 <- drop_empty_rc(m)
m2

}

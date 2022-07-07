\name{cosine_dist}
\alias{cosine_dist}
\title{
Calculate cosine-based distances between rows of a matrix
}
\description{
The function \code{cosine_dist} takes as its argument an m-by-n matrix
 \code{x}, and
 returns all cosine-based distances between the rows in that matrix.
 Rows in \code{x} are interpreted as position vectors in n-dimensional space.
 The cosine-based distance between two rows is based on the angle between these
 two position vectors, and more precisely is calculated as one minus the
 cosine of that angle.
}
\usage{
cosine_dist(x) 

}
\arguments{
  \item{x}{
  the m-by-n input matrix \code{x}.
}
}
\value{
Returns a square matrix containing the distances between all the rows
  from the input matrix \code{x}.
}
\examples{
## first example
m <- matrix(nrow = 3, byrow = TRUE,
            dimnames = list(c('r1','r2','r3'),
                            c('c1','c2','c3')),
            c(.1,.2,.4,
              .2,.2,.6,
              .8,.3, 2))
cosine_dist(m)

## second example
r <- 1
theta <- pi/2 * .9 # angle in radians
m <- matrix(nrow = 2, byrow = TRUE,
                   c(1, 0,
                     r*cos(theta), r*sin(theta)))
cosine_dist(m)
1 - cos(theta)

}

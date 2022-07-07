\name{xlim4ca}
\alias{xlim4ca}
\alias{ylim4ca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Range of Coordinates to be used in Correspondence Analysis Plot
}
\description{
  The function \code{xlim4ca} 
  takes as its input an object of class "ca", and returns the 
  range of principal coordinates to be used in the first dimension
  of the correspondence analysis plot.
  The function \code{ylim4ca} 
  takes as its input an object of class "ca", and returns the 
  range of principal coordinates to be used in the first dimension
  of the correspondence analysis plot.
}
\usage{
xlim4ca(x, ...)
ylim4ca(x, ...)
}
\arguments{
  \item{x}{
   object of class \code{"ca"}
  }
  \item{...}{
   (not used for the moment)
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  The function \code{xlim4ca} returns a numeric vector of length two
  containing the lower and upper boundary of the x-axis to be used
  in a biplot of the correspondence analysis solution in \code{object}.

  The function \code{ylim4ca} returns a numeric vector of length two
  containing the lower and upper boundary of the y-axis to be used
  in a biplot of the correspondence analysis solution in \code{x}.

}
\examples{

## traditional biplot, using the basic tools from the
## package ca
library(ca)
data("author") # available from package ca
author_ca <- ca(author)
plot(author_ca)

## alternative plot, using the coordinates retrieved with
## row_pcoord() and col_pcoord()
r_pc <- row_pcoord(author_ca)
c_pc <- col_pcoord(author_ca)
xlim <- xlim4ca(author_ca)     # range of dim 1
ylim <- ylim4ca(author_ca)     # range of dim 2
author_names <- as.factor(gsub("^.*?\\\\((.*?)\\\\)$", "\\\\1",
  rownames(author), perl = TRUE))
plot(r_pc[,1], r_pc[,2], pch = 18, 
     xlim = xlim, ylim = ylim, xlab = "", ylab = "",
     main = "authors and their alphabet",
     col = as.numeric(author_names))
abline(h = 0, col = "gray", lty = 3)
abline(v = 0, col = "gray", lty = 3)
text(c_pc[,1], c_pc[,2], colnames(author), col = "gray")
legend("topright", 
       legend = levels(author_names),  
       pch = rep(18, length(levels(author_names))),
       col = 1:length(levels(author_names)), 
       title = "authors")

}

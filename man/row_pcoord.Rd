\name{row_pcoord}
\alias{row_pcoord}
\alias{col_pcoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Row Principal Coordinates or Column Principal Coordinates
}
\description{
  The function \code{row_pcoord} 
  takes as its input an object of class "ca", and returns the 
  row principal coordinates found in that object.
  The functions \code{col_pcoord}  
  takes as its input an object of class "ca", and returns the 
  column principal coordinates found in that object.
}
\usage{
row_pcoord(x, ...)
col_pcoord(x, ...)
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
  The function \code{row_pcoord} returns a table
  in which the rows represent the `row objects' from the correspondence
  analysis, and the columns store the principal coordinates of these
  `row objects' in the different dimensions of the correspondence
  analysis solution.

  The functions \code{col_pcoord} returns a table
  in which the rows represent the `column objects' from the correspondence
  analysis, and the columns store the principal coordinates of these `column
  objects' in the different dimensions of the correspondence analysis solution.

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

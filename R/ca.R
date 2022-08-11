#' Helpers for plotting `ca` objects
#'
#' The functions `row_pcoord()` and `col_pcoord()` retrieve the coordinates of
#' the rows and columns of a [`ca`][ca::ca()] object across all dimensions.
#' The functions `xlim4ca()` and `ylim4ca()` return the range of values for the
#' first and second dimensions.
#' 
#' In the output of `row_pcoord()`, each row corresponds to a row from the dataframe
#' that [ca::ca()] was applied to, and each column corresponds to a principal component.
#' In the output of `col_pcoord()`, each row corresponds to a colum from the dataframe
#' that [ca::ca()] was applied to, and each column corresponds to a principal component.
#' 
#' @param x An object of class [`ca`][ca::ca()].
#' @param ... Additional arguments (not implemented).
#'
#' @return A matrix (for `row_pcoord()` and `col_pcoord()`) or a numeric vector
#'   (for `xlim4ca()` and `ylim4ca()`).
#' @export
#' @name ca_help
#'
#' @examples
#' 
#' # traditional biplot from {ca}
#' 
#' library(ca)
#' data("author")
#' author_ca <- ca(author)
#' plot(author_ca)
#' 
#' # alternative plot with {mclm} tools
#' r_pc <- row_pcoord(author_ca)
#' c_pc <- col_pcoord(author_ca)
#' xlim <- xlim4ca(author_ca)
#' ylim <- ylim4ca(author_ca)
#' author_names <- as.factor(gsub(
#'                               "^.*?\\((.*?)\\)$", "\\1",
#'                              rownames(author), perl = TRUE))
#' plot(r_pc[,1], r_pc[,2], pch = 18,
#'     xlim = xlim, ylim = ylim, xlab = "", ylab = "",
#'     main = "authors and their alphabet",
#'     col = as.numeric(author_names))
#' abline(h = 0, col = "gray", lty = 3)
#' abline(v = 0, col = "gray", lty = 3)
#' text(c_pc[,1], c_pc[,2], colnames(author), col = "gray")
#' legend("topright",
#'        legend = levels(author_names),
#'        pch = rep(18, length(levels(author_names))),
#'        col = 1:length(levels(author_names)),
#'        title = "authors")
NULL

#' @describeIn ca_help Retrieve row principal coordiantes for all dimensions
row_pcoord <- function(x, ...) {
  x$rowcoord %*% diag(x$sv)
}

#' @describeIn ca_help Retrieve column principal coordiantes for all dimensions
col_pcoord <- function(x, ...) {
  x$colcoord %*% diag(x$sv)
}

#' @describeIn ca_help Return range of first dimension for plotting
xlim4ca <- function(x, ...) {
  r_pc <- row_pcoord(x, ...)
  c_pc <- col_pcoord(x, ...)
  range(r_pc[,1], c_pc[,1])
}

#' @describeIn ca_help Return range of second dimension for plotting
ylim4ca <- function(x, ...) {
  r_pc <- row_pcoord(x, ...)
  c_pc <- col_pcoord(x, ...)
  xlim <- range(r_pc[,1], c_pc[,1])
  ylim <- range(r_pc[,2], c_pc[,2])
  xr <- xlim[2] - xlim[1]
  yr <- ylim[2] - ylim[1]
  r_diff <- xr - yr
  c(ylim[1] - r_diff/2, ylim[2] + r_diff/2) 
}


# ------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR EXPLAINING/EXPLORING CORRESPONDENCE ANALYSIS
# ------------------------------------------------------------------------------

ca.row.profiles <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the row profiles matrix R for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat), 1))
}

ca.col.profiles <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the col profiles matrix C for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat), 2))
}

ca.corresp.matrix <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the correspondence matrix P for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat)))
}

ca.row.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the row centroid for the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  return(data.c)  
}

ca.col.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the column centroid for the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  return(data.r)
}

ca.plot.profiles <- function(tdat, side=TRUE, vertical=TRUE) {
# ------------------------------------------------------------------------------
# Plots row and columns profiles for the two way contingency table tdat
# ------------------------------------------------------------------------------
# side=TRUE means plotting the two plots side by side
# side=FALSE means plotting one above the other
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  dir <- c("v","h"); if (!vertical) { dir <- c("h","v") }
  if (side) { par(mfrow=c(1,2)) } else { par(mfrow=c(2,1)) }
  graphics::mosaicplot(tdat, col=T, main="row profiles", dir=dir) 
  graphics::mosaicplot(t(tdat), col=T, main="column profiles",dir=dir) 
  par(mfrow=c(1,1))
}

ca.row.masses <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates masses for the rows in the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses
  return(data.r)
}

ca.col.masses <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates masses for the columns in the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.c    <- apply(data.P, 2, sum)     # column masses
  return(data.c)
}

h.calc.chisq.dist <- function(x1, x2, m) {
# ------------------------------------------------------------------------------
# [auxiliary function used by row.chisq.dists() and col.chisq.dists()]
# calculated chi-square distance between two profiles x1 and x2, using m
# as the masses by means of which to weight the components in x1 and x2 
# ------------------------------------------------------------------------------
# if tdat is a matrix with absolute frequencies, then row distances between 
# the profiles of row 1 and row 2 are computed as follows:
#   data.P   <- tdat / sum(tdat)           # correspondence matrix  
#   row.prof <- prop.table(data.P, 1)      # row profiles
#   data.c   <- apply(data.P, 2, sum)      # col masses 
#   h.calc.chisq.dist(row.prof[1,], row.prof[2,], data.c)
# ------------------------------------------------------------------------------
# if tdat is a matrix with absolute frequencies, then column distances between 
# the profiles of column 1 and column 2 are computed as follows:
#   data.P   <- tdat / sum(tdat)           # correspondence matrix  
#   col.prof <- prop.table(data.P, 2)      # column profiles
#   data.r   <- apply(data.P, 1, sum)      # row masses 
#   h.calc.chisq.dist(col.prof[,1], col.prof[,2], data.r)
# ------------------------------------------------------------------------------
  result <- 0
  for (i in 1:length(x1)) {
    result <- result + (1/m[i] * (x1[i] - x2[i])^2)
  }
  return(sqrt(result))
}

row.chisq.dists <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all chi-square row distances between the rows in the 
# two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles
  data.c    <- apply(data.P, 2, sum)     # col masses 
  data.r    <- apply(data.P, 1, sum)     # row masses
  m <- outer(data.r, data.r) * 0         # initialize function output
  for (i in 1:nrow(data.P)) {
    for (j in 1:nrow(data.P)) {
      m[i,j] <- h.calc.chisq.dist(row.prof[i,], row.prof[j,], data.c)
    }
  }
  return(m)
}

col.chisq.dists <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all chi-square column distances between the columns in the 
# two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof <- prop.table(data.P, 2)      # col profiles  
  data.c    <- apply(data.P, 2, sum)     # col masses 
  data.r    <- apply(data.P, 1, sum)     # row masses
  m <- outer(data.c, data.c) * 0         # initialize function output 
  for (i in 1:ncol(data.P)) {
    for (j in 1:ncol(data.P)) {
      m[i,j] <- h.calc.chisq.dist(col.prof[,i], col.prof[,j], data.r)
    }
  }
  return(m)
}

inertia.contrib <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the contributions in all cells of the two way contingency table
# tdat to the overall inertia.
# ------------------------------------------------------------------------------
# This overall inertia is equal to
#      chisq.test(tdat, correct=F)$statistic / sum(tdat)
# and the contributions to the overall inertia are equal to
#      (chisq.test(tdat, correct=F)$residuals^2)/sum(tdat)
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  exp.mass  <- outer(data.r, data.c)     # expected masses (given independence)
  result <- outer(data.r, data.c) * 0
  for (i in 1:nrow(tdat)) {
    for (j in 1:ncol(tdat)) {
       result[i,j] <- 
         ((data.P[i,j] - exp.mass[i,j])^2 / exp.mass[i,j])
    }
  }
  return(result)
}

row.inertias <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- 0
    for (j in 1:ncol(tdat)) {
      result[i] <- result[i] + ((row.prof[i,j] - data.c[j])^2 / data.c[j])
    }
    result[i] <- result[i] * data.r[i]   # multiply by row mass
  }
  return(result)
}

col.inertias <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- 0
    for (i in 1:nrow(tdat)) {
      result[j] <- result[j] + ((col.prof[i,j] - data.r[i])^2 / data.r[i])
    }
    result[j] <- result[j] * data.c[j]   # multiply by column mass
  }
  return(result)
}


row.inertias2 <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- 0
    row.dist.to.centroid <- h.calc.chisq.dist(row.prof[i,], data.c, data.c) 
    row.mass <- data.r[i]
    result[i] <- row.dist.to.centroid^2 * row.mass
  }
  return(result)
}

col.inertias2 <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- 0
    col.dist.to.centroid <- h.calc.chisq.dist(col.prof[,j], data.r, data.r) 
    col.mass <- data.c[j]
    result[j] <- col.dist.to.centroid^2 * col.mass
  }
  return(result)
}

row.dists.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- h.calc.chisq.dist(row.prof[i,], data.c, data.c) 
  }
  return(result)
}

col.dists.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- h.calc.chisq.dist(col.prof[,j], data.r, data.r) 
  }
  return(result)
}

fitted.row.dists <- function(tdat.ca) {
# ------------------------------------------------------------------------------
# Calculates all euclidean row distances in the first two dimensions of the
# ca solution tdat.ca using the principal row coordinates
# ------------------------------------------------------------------------------
  princ.coord <- tdat.ca$rowcoord %*% diag(tdat.ca$sv)
  x           <- rep(0, length(tdat.ca$rownames))
  names(x)    <- tdat.ca$rownames
  result      <- outer(x, x)                        # initialize function output
  for (i in 1:nrow(princ.coord)) {
    for (j in 1:nrow(princ.coord)) {
      y <- 0
      for (k in 1:2) {
        y = y + (princ.coord[i,k] - princ.coord[j,k])^2
      }
      result[i,j] <- sqrt(y)
    }
  }
  return(result)
}

fitted.col.dists <- function(tdat.ca) {
# ------------------------------------------------------------------------------
# Calculates all euclidean column distances in the first two dimensions of the
# ca solution tdat.ca using the principal column coordinates
# ------------------------------------------------------------------------------
  princ.coord <- tdat.ca$colcoord %*% diag(tdat.ca$sv)
  x           <- rep(0, length(tdat.ca$colnames))
  names(x)    <- tdat.ca$colnames
  result      <- outer(x, x)                        # initialize function output
  for (i in 1:nrow(princ.coord)) {
    for (j in 1:nrow(princ.coord)) {
      y <- 0
      for (k in 1:2) {
        y = y + (princ.coord[i,k] - princ.coord[j,k])^2
      }
      result[i,j] <- sqrt(y)
    }
  }
  return(result)
}




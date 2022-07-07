\name{keep_fnames}
\alias{keep_fnames}
\alias{drop_fnames}
\title{
Filter collection of filenames by names
}
\description{
Build a subset of an \code{'fnames'} object \code{x}, using a
 set of names \code{y} as filter criterion. In the 
 case of \code{keep_fnames} (with \code{invert} set to its default value
 \code{FALSE}), items from \code{x} are only
 kept if they also appear in \code{y}. In the 
 case of \code{drop_fnames} (but also in the case of 
 \code{keep_fnames} with \code{invert} set to \code{TRUE}), items from \code{x} are only
 kept if they DO NOT appear in \code{y}.
}
\usage{
drop_fnames(x, y, \dots)
keep_fnames(x, y, invert = FALSE, \dots)
}
\arguments{
  \item{x}{
  an \code{'fnames'} object. This is the object that is to be filtered.
  }
  \item{y}{
  an \code{'fnames'} object or a \code{'types'} object or any 
  character vector. This is the filter criterion.
  }
  \item{invert}{
  if this boolean value is set to \code{TRUE}, then \code{'keep_fnames'} 
  behaves like \code{'drop_fnames'}.
  }
  \item{\dots}{
  additional arguments.
  }
}
\value{
Returns an \code{'fnames'} object.
}

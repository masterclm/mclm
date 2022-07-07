\name{details.default}
\alias{details.default}
\title{
Zoom in, within an aggregate-level analysis, on a specific linguistic item
}
\description{
Generic function that takes as its first argument \code{x}
  an object that represents an aggregate-level analysis,
  for instance an object of the
  class \code{"slma"}, and extracts from it detailed information
  related to the linguistic item \code{y}.
}
\usage{
\method{details}{default}(x, y)
}
\arguments{
  \item{x}{
  an object that represents an aggregate-level analysis,
  for instance an object of the
  class \code{"slma"} 
  }
  \item{y}{
  a character vector (assumed to be of length 1) containing a
  linguistic item (typically a lexical item)
  }
}
\value{
Returns an object containing detailed information regarding the
linguistic item \code{y}.
}

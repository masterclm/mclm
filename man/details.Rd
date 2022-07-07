\name{details}
\alias{details}
\title{
Inspect details, within a global analysis, for a specific item
}
\description{
Generic function that takes as its first argument \code{x}
  an object that contains global statistics for
  a collection of linguistic units, for instance an object of the
  class \code{"slma"}, and extracts from it detailed information
  for a specific (lexical) item.
}
\usage{
details(x, y)
}
\arguments{
  \item{x}{
  an object that contains global statistics for
  a collection of linguistic units, for instance an object of the
  class \code{"slma"} 
  }
  \item{y}{
  a character vector (assumed to be of length 1) that represents one
  linguistic item, typically a lexical item.
  }
}
\value{
Returns an object containing detailed information regarding item
 \code{y}.
}

\name{print.types}
\alias{print.types}
\title{
Print a Vector of Types
}
\description{
Print objects of the class \code{'types'}. 
}
\usage{
\method{print}{types}(x,
      n = 20,
      from = 1,
      sort_order = c("none", "alpha"),
      extra = NULL,
      \dots)
}
\arguments{
  \item{x}{
  an object of the class \code{'types'}
  }
  \item{n}{
  maximum number of types to print
  }
\item{from}{
  position of the first item to print
  }
  \item{sort_order}{
  order in which the items are to be printed. Possible values
  are \code{"alpha"}
  (meaning that the items are to be sorted alphabetically), and
  \code{"none"} (meaning that the items are not to be sorted).
  }
  \item{extra}{
  extra settings
  }
  \item{\dots}{
  additional printing arguments.
  }
}
\value{
Invisibly returns \code{x}.
}
\examples{
x <- as_types(c("first", "second", "third"))
print(x, n = 1000)

}

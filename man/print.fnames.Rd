\name{print.fnames}
\alias{print.fnames}
\title{
Print a Vector of Filenames
}
\description{
Print objects of the class \code{'fnames'}. 
}
\usage{
\method{print}{fnames}(x,
      n = 20,
      from = 1,
      sort_order = c("none", "alpha"),
      extra = NULL,
      \dots)
}
\arguments{
  \item{x}{
  an object of the class \code{'fnames'}
  }
  \item{n}{
  maximum number of items to print
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
cwd_fnames <- get_fnames(recursive = FALSE)
print(cwd_fnames, n = 1000)

}

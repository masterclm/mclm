\name{print_kwic}
\alias{print_kwic}
\title{
Print a concordance in KWIC format.
}
\description{
Print a concordance in KWIC format.
}
\usage{
print_kwic(x, 
           min_c_left = NA,
           max_c_left = NA,
           min_c_match = NA,
           max_c_match = NA,
           min_c_right = NA,
           max_c_right = NA,
           from = 1,
           n = 30,
           drop_tags = TRUE)
}
\arguments{
  \item{x}{
  a dataset 
  }
  \item{min_c_left}{
  minimum size, expressed in number of characters, of the left co-text in
  the KWIC display.
  }
  \item{max_c_left}{
  maximum size, expressed in number of characters, of the left co-text in
  the KWIC display.
  }
  \item{min_c_match}{
  minimum size, expressed in number of characters, of the match in the
  KWIC display.
  }
  \item{max_c_match}{
  maximum size, expressed in number of characters, of the match in the
  KWIC display.
  }
  \item{min_c_right}{
  minimum size, expressed in number of characters, of the right co-text in
  the KWIC display.
  }
  \item{max_c_right}{
  maximum size, expressed in number of characters, of the right co-text in
  the KWIC display.
  }
  \item{from}{
  index of first item in \code{x} that is to be displayed.
  }
  \item{n}{
  number of consecutive items in \code{x} that are to be displayed.
  }
  \item{drop_tags}{
  boolean: hide tags in data?
  }

}
\details{
  This procedure assumes that \code{x} is either an object of the class
  \code{conc} or any other data frame that contains textual data in three
  columns named \code{left}, \code{match}, and \code{right}.
}
\value{
Invisibly returns \code{x}.
}
\examples{
(conc_data <- conc('A very small corpus.', '\\\\w+', as_text = TRUE))
print_kwic(conc_data)
}


\name{summary.re}
\alias{summary.re}
\alias{print.summary.re}
\title{
Succinct Description of a Regular Expression
}
\description{
Build and/or print an object of the class \code{'summary.re'}.
}
\usage{

\method{summary}{re}(object, \dots)
\method{print}{summary.re}(x, \dots)
 
}
\arguments{
  \item{object}{
   an object of the class \code{'re'}.
  }
  \item{x}{
   an object of the class \code{'summary.re'}, which is the type of object that
   is returned by the \code{summary} method described on this page.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The methods described on this page return an object of the class
  \code{'summary.re'}.
}

\examples{

summary(re("^.{3,}")) 

print(summary(re("^.{3,}"))) 

}

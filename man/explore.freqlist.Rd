\name{explore.freqlist}
\alias{explore.freqlist}
\title{
Interactively Navigate Through Frequency List
}
\description{
The S3 method \code{explore()}, which only works in an interactive R
  session, can be applied to frequency lists. In that case, the method takes
  as its first argument an R object \code{x} of the
  class \code{"freqlist"}, and it
  opens `exploration mode', in which the user can navigate
  through \code{x} by means of brief commands. Inside `exploration mode',
  the user can ask for the
  list of available commands by keying in \code{?}, followed by ENTER.
  The user can quit `exploration mode' by keying in \code{q}, followed
  by ENTER.
}
\usage{
\method{explore}{freqlist}(x,
   n = 20,
   from = 1,
   perl = TRUE,
   use_clear = TRUE,
   \dots)

}
\arguments{
  \item{x}{
   an object of the classes \code{"freqlist"}.
  }
  \item{n}{
    maximum number of items in the frequency list to be shown at once.   
  }
  \item{from}{
    first item in the frequency list that is shown at the beginning of
    the ineractive session.
  }
  \item{perl}{
    boolean value that indicates whether or not regular expressions used
    in the exploration session use the PERL flavor of regular expression.
  }
  \item{use_clear}{
    boolean value. If \code{use_clear} is \code{TRUE}, and if moreover the
    feature is supported by the R environment, the console will be cleared
    in between all interactive steps in the exploration session.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{explore} invisibly returns \code{x}.
}

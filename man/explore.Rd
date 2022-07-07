\name{explore}
\alias{explore}
\title{
Interactively Navigate Through Object
}
\description{
The generic method \code{explore}, which only works in an interactive R
  session, takes as its first argument an R object \code{x}, and
  opens an `exploration mode' in which the user can navigate
  through \code{x} by means of brief commands.
  Inside `exploration mode',
  the user can ask for the
  list of available commands by keying in \code{?}, followed by ENTER.
  The user can quit `exploration mode' by keying in \code{q}, followed
  by ENTER.
}
\usage{
explore(x, \dots)
}
\arguments{
  \item{x}{
   an object of any of the classes for which the method \code{explore}
   is implemented.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The method \code{explore} invisibly returns \code{x}.
}

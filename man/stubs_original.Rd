\name{stubs}
\alias{stubs}
\alias{plot.freqlist}
\alias{plot.re}
\alias{plot.tokens}
\alias{plot.summary.freqlist}
\alias{plot.summary.re}
\alias{plot.summary.tokens}
\alias{n_tokens.default}
\alias{n_types.default}
\alias{type_names.default}
\title{
Stubs (Functions/Methods Not Implemented)
}
\description{
The \code{mclm} package contains a number of methods which don't do
  anything useful at all, except from producing a warning, either because useful
  implementations are yet to be implemented or because we simply couldn't think
  of a useful implementation.
  
  Typical examples are (i) defaults of generic methods that are only
  meaningful for a limited number of classes, e.g. \code{n_tokens} and
  \code{n_types}, and (ii) specific instances
  of generic methods which target classes
  for which there is no implementation of the method (yet), e.g.
  certain instances of the plotting method \code{\link{plot}}.
  
  The methods listed on this page have in common that they
  don't do anything substantial
  (e.g. they don't return a meaningful number or they don't
  produce a plot) and warn the user about this. Producing a
  clear warning is the sole purpose of these methods. 
}
\usage{

\method{plot}{freqlist}(x, \dots)

\method{plot}{re}(x, \dots)

\method{plot}{tokens}(x, \dots)

\method{plot}{types}(x, \dots)

\method{plot}{summary.freqlist}(x, \dots)

\method{plot}{summary.re}(x, \dots)

\method{plot}{summary.tokens}(x, \dots)

\method{plot}{summary.types}(x, \dots)

\method{n_tokens}{default}(x, \dots)

\method{n_types}{default}(x, \dots)

\method{type_names}{default}(x, \dots)


}
\arguments{
  \item{x}{
   an object of some specific \code{mclm} class.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
All \code{plot} methods listed on this page invisibly return the value
  \code{NULL}. The default \code{n_tokens}, \code{n_types}, and
  \code{type_names} methods
  returns \code{NA}.
}
\details{
  The methods listed on this page are implemented in order to prevent
  cryptic error messages or warning from being show when the user
  (accidentally) calls any of these methods.
}
\examples{

plot(re("^.{3,}"))

plot(summary(re("^.{3,}")))

n_tokens(1:10)

n_tokens(as_types(c("the", "a", "it")))

n_types(1:10)

n_types(as_tokens(c("a", "tiny", "example")))

type_names(1:10)


}

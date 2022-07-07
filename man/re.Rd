\name{re}
\alias{re}
\alias{as_re}
\alias{as.re}
\alias{print.re}
\title{
Build a Regular Expression
}
\description{
Build and/or print an object of the class 're'.
}
\usage{
re(x, perl = TRUE, \dots)

as_re(x, perl = TRUE, \dots)

as.re(x, perl = TRUE, \dots)

\method{print}{re}(x, \dots)

}
\arguments{
  \item{x}{
   a character vector of length one. The value of this character vector
   is assumed to be a well-formed regular expression. In the current
   implementation, this is simply assumed, not checked.
  }
  \item{perl}{
   a logical vector of length one. If \code{perl} is \code{TRUE},
   then the regular expression in \code{x} is assumed to use
   PCRE (i.e. Perl Compatible Regular Expressions) notation.
   If \code{perl} is \code{FALSE},
   then the regular expression in \code{x} is assumed to use
   base R's default regular expression notation.
   Contrary to the regular expression functions in base R, the function
   \code{re} by default assumes that the PCRE regular expression flavor
   is used.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The functions described on this page return an object of the class
  \code{'re'}.
}
\details{
An object of the class \code{'re'} is nothing but a `wrapper' around a
  length one character vector that
  is `flagged' as containing a regular expression. The class \code{'re'}
  exists because
  some functions in the
  \code{mclm} package require that their arguments are explicitly marked
  as being regular expressions. See below, in the examples section, for
  examples of such cases.
}
\seealso{
\code{\link{perl_flavor}}, \code{\link{scan_re}}, \code{\link{cat_re}}.  
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
(tks <- tokenize(toy_corpus))

keep_re(tks, re("^.{3,}")) # here, the use of re() is optional
keep_re(tks, "^.{3,}")     # same as previous instruction

tks[re("^.{3,}")]          # here, the use of re() is necessary
tks["^.{3,}"]              # wrong! here, "^.{3,}" is not treated as a regex

re("^.{3,}")               # build and print a 're' object
as_re("^.{3,}")            # build and print a 're' object
as.re("^.{3,}")            # build and print a 're' object
print(re("^.{3,}"))        # build and print a 're' object


}

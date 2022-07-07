\name{scan_re}
\alias{scan_re}
\title{
Scan a Regular Expression from Console
}
\description{
The function \code{scan_re} can be used to scan a regular expression
 from the console.
 After the function call, R will continue scanning your input
 until it encounters an empty input line, i.e. until it encounters
 two consecutive newline symbols (or until it encounters a line with
 nothing but whitespace characters). In other words, press ENTER
 twice in a row if you want to stop inputting characters.
 The function will then return your input as an object of
 the class \code{'re'}.
}
\usage{
scan_re(perl = TRUE, \dots)

}
\arguments{
  \item{perl}{
   a logical vector of length one. If \code{perl} is \code{TRUE},
   then the regular expression being scanned is assumed to use
   PCRE (i.e. Perl Compatible Regular Expressions) notation.
   If \code{perl} is \code{FALSE},
   then the regular expression being scanned is assumed to use
   base R's default regular expression notation.
   Contrary to the regular expression functions in base R, the function
   \code{scan_re} by default assumes that the PCRE regular expression flavor
   is used.
  }
  \item{\dots}{
   additional arguments.
  }
}
\details{
 The specific reason why the function  \code{scan_re}
 was created, is to be able to avoid
 the need for doubling backslashes when inputting regular expressions.
 When regular expressions are in a string literal, the special
 meaning of backslashes in R string literals forces one to
 double backslashes, which is especially inconvenient because
 backslashes feature prominently in regular expression notation.
 The function \code{scan_re} interprets
 its input as a `raw string'. Therefore, there is no need for
 doubling backslashes.
}
\value{
Returns an object of the class \code{'re'}.
}
\seealso{
\code{\link{re}}, \code{\link{cat_re}}.  
}

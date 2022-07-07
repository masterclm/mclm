\name{scan_txt}
\alias{scan_txt}
\title{
Scan a Character String from Console
}
\description{
The function \code{scan_txt}, which takes
 no arguments, can be used to scan a text string from the console.
 After the function call, R will continue scanning your input
 until it encounters an empty input line, i.e. until it encounters
 two consecutive newline symbols (or until it encounters a line with
 nothing but whitespace characters). In other words, press ENTER
 twice in a row if you want to stop inputting characters.
 The function will then return your input as a character vector
 of length one. 
}
\usage{
scan_txt()
}
\value{
Returns a character vector of length one that contains the string that has
 been scanned.
}
\seealso{
See also: \code{\link{scan_re}}.  
}

\name{cat_re}
\alias{cat_re}
\title{
Print a regular expression to the console
}
\description{
The function \code{cat_re} prints a regular expression to the console.
 By default, the regular expression is not printed as
 an R string, but as a `plain regular expression'. More
 specifically, the regular expression is printed without surrounding
 quotation marks, and characters that are special characters in
 R strings (such as quotation marks and backslashes) are not escaped with
 a backslash. Also, by default, multi-line regular expressions are
 printed as single-line regular expressions with all regular expression
 comments removed. 
}
\usage{
cat_re(x,
       format = c("plain", "R"),
       as_single_line = TRUE)
}
\arguments{
  \item{x}{
   either an object of the class \code{'re'} or
   a character vector containing a regular expression. If \code{x}
   is a character vector the length of which is
   higher than 1, then only \code{x[1]} is taken into account.
  }
  \item{format}{
   a character vector describing the requested output format. If the
   length of \code{format} is higher than 1, then only \code{format[1]} is
   taken into account.
  }
  \item{as_single_line}{
   a boolean vector specifying whether the regular expression should be
   converted to a single-line regular expression (which also implies
   removing all regular expression comments) prior to printing.
   If the length of \code{as_single_line} is higher than 1, then only
   \code{as_single_line[1]} is taken into account.
  }
}
\details{
In the current implementation, the way the character \# is handled is
   not guaranteed to be correct. More specifically, the code is not guaranteed
   to correctly distinguish between a \# symbol that introduces a regular
   expression comment and a \# symbol that doesn't do so. Firstly,
   there is no testing whether at the point of encountering \# we're in
   free-spacing mode. Second, there is no thorough testing whether or not
   the \# symbol is part of a character class. 
   However, \# is processed correctly as long as any `literal #' is 
   immediately preceded by either a backslash or an opening square bracked,
   and any `comment-introducing #' is not immediately preceded by
   a backslash or an opening square bracket.
}
\value{
The (invisible) value of \code{cat_re} is the same as its first argument, viz.
 the argument \code{x}.
}
\seealso{
\code{\link{scan_re}}.
}
\examples{
# single-line regular expression
x <- "(?xi)  \\\\b \\\\w* willing \\\\w* \\\\b"
cat_re(x)

# multi-line regular expression
y <- "(?xi)  
       \\\\b        # word boundary 
       \\\\w*       # optional prefix
       willing   # stem
       \\\\w*       # optional suffix
       \\\\b        # word boundary"
cat_re(y)
cat_re(y, as_single_line = FALSE)
cat_re(y, format = "R")
cat_re(y, format = "R", as_single_line = FALSE)

regex <- re("(?xi)  
                \\\\b        # word boundary 
                \\\\w*       # optional prefix
                willing   # stem
                \\\\w*       # optional suffix
                \\\\b        # word boundary")
cat_re(regex)
cat_re(regex, as_single_line = FALSE)

}

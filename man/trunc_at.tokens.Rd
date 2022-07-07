\name{trunc_at.tokens}
\alias{trunc_at.tokens}
\title{
Truncates sequence of character data at position matching pattern
}
\description{
S3 function that, when applied to an object \code{x}
  of the class \code{'tokens'}, truncates it at the position where a 
  match for the argument \code{pattern} is found.
}
\usage{
\method{trunc_at}{tokens}(x, 
                           pattern, 
                           keep_this = FALSE, 
                           last_match = FALSE, 
                           from_end = FALSE,
                           \dots)
}
\arguments{
  \item{x}{
  an object of the
  class \code{'tokens'}. 
  }
  \item{pattern}{
  a regular expression. 
  }
  \item{keep_this}{
  a boolean value specifying whether or not the matching token itself is kept, i.e.
  whether truncating happens right after the matching token (\code{keep_this} is \code{TRUE})
  or right before the matching token (\code{keep_this} is \code{TRUE})
  }
  \item{last_match}{
  a boolean value specifying, in case there are several matching tokens, whether the first
  match is used as truncation point (\code{last_match} is \code{FALSE})
  or the last match (\code{last_match} is \code{TRUE})
  }
  \item{from_end}{
  a boolean value specifying whether the search for a match starts from the first token, progressing
  forward (\code{from_end} is \code{FALSE}), or starts from the last token, progressing
  backward (\code{from_end} is \code{TRUE})
  
  If \code{from_end} is \code{FALSE}, the part of \code{x} that is kept after truncation,
  is the head of \code{x}.
  If \code{from_end} is \code{TRUE}, then the part of \code{x} that is kept after truncation,
  is the tail of \code{x}.
  
  }
  \item{\dots}{
  additional arguments. 
  }
}
\value{
Returns a truncated version of \code{x}.
}
\examples{
(toks <- tokenize('This is a first sentence . This is a second sentence .',
                  re_token_splitter = '\\\\s+'))

trunc_at(toks, re("[.]"))

trunc_at(toks, re("[.]"), last_match = TRUE)

trunc_at(toks, re("[.]"), last_match = TRUE, from_end = TRUE)

}

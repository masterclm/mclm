\name{as_types}
\alias{as_types}
\title{
Coerce object to a vector of types
}
\description{
Coerces any object \code{x} to a vector of types
}
\usage{
as_types(x,
         remove_duplicates = TRUE,
         sort = TRUE,
         \dots)
}
\arguments{
  \item{x}{
   the object \code{x} can be any type of object.
  }
  \item{remove_duplicates}{
   length one boolean vector that determines whether or not
   duplicates are removed from \code{x} prior to coercing
   to a vector of types
  }
  \item{sort}{
   length one boolean vector that determines whether or not
   \code{x} is alphabetically sorted prior to coercing
   to a vector of types; this argument is ignored if
   \code{remove_duplicates} is \code{TRUE}, because the
   result of removing duplicates is always sorted.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
This function returns an object of the class \code{'types'}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
flist <- freqlist(toy_corpus, re_token_splitter = "\\\\W+", as_text = TRUE)
print(flist, n = 1000)
(sel_types <- as_types(c("happily", "lived", "once")))
keep_types(flist, sel_types)

tks <- tokenize(toy_corpus, re_token_splitter = "\\\\W+")
print(tks, n = 1000)
tks[3:12] # idx is relative to selection
head(tks) # idx is relative to selection
tail(tks) # idx is relative to selection

}

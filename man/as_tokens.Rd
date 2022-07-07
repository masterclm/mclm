\name{as_tokens}
\alias{as_tokens}
\title{
Coerce object to a vector of tokens
}
\description{
Coerces any object \code{x} to a vector of tokens
}
\usage{
as_tokens(x,
          ...)
}
\arguments{
  \item{x}{
   the object \code{x} can be any type of object.
  }
  \item{...}{
   additional arguments (not used in the present implementation).
  }
}
\value{
This function returns an object of the class \code{'tokens'}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
tokens <- tokenize(toy_corpus, re_token_splitter = "\\\\W+")
print(tokens, n = 1000)
tokens[3:12]
print(as_tokens(tokens[3:12]), n = 1000)
as_tokens(tail(tokens))

}

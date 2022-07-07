\name{print.tokens}
\alias{print.tokens}
\title{
Print a Vector of Tokens
}
\description{
Print objects of the class \code{'tokens'}. 
}
\usage{
\method{print}{tokens}(x,
      n = 20,
      from = 1,
      extra = NULL,
      \dots)
}
\arguments{
  \item{x}{
  an object of the class \code{'tokens'}.
  }
  \item{n}{
  maximum number of tokens to print.
  }
  \item{from}{
  position in \code{x} of the first token to be printed.
  }
  \item{extra}{
  extra settings
  }
  \item{\dots}{
  additional printing arguments.
  }
}
\value{
Invisibly returns \code{x}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
tks <- tokenize(toy_corpus, re_token_splitter = "\\\\W+")
print(tks, n = 1000)
tks[3:12]
tail(tks)

}


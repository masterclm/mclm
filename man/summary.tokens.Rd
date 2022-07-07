\name{summary.tokens}
\alias{summary.tokens}
\alias{print.summary.tokens}
\title{
Succinct Description of a 'tokens' Object
}
\description{
Build and/or print an object of the class \code{'summary.tokens'}.
}
\usage{

\method{summary}{tokens}(object, \dots)
\method{print}{summary.tokens}(x, \dots)
 
}
\arguments{
  \item{object}{
   an object of the class \code{'tokens'}.
  }
  \item{x}{
   an object of the class \code{'summary.tokens'}, which is the
   type of object that
   is returned by the \code{summary} method described on this page.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The functions described on this page return an object of the class
  \code{'summary.tokens'}.
}

\examples{

tks <- tokenize("the old man and the sea.")
summary(tks) 
print(summary(tks))

(tks_sum <- summary(tks))
names(tks_sum)
tks_sum[["n_tokens"]]
tks_sum$n_tokens

}

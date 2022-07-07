\name{summary.freqlist}
\alias{summary.freqlist}
\alias{print.summary.freqlist}
\title{
Succinct Description of a 'freqlist' Object
}
\description{
Build and/or print an object of the class \code{'summary.freqlist'}.
}
\usage{

\method{summary}{freqlist}(object, \dots)
\method{print}{summary.freqlist}(x, \dots)
 
}
\arguments{
  \item{object}{
   an object of the class \code{'freqlist'}.
  }
  \item{x}{
   an object of the class \code{'summary.freqlist'}, which is the
   type of object that
   is returned by the \code{summary} method described on this page.
  }
  \item{\dots}{
   additional arguments.
  }
}
\value{
The functions described on this page return an object of the class
  \code{'summary.freqlist'}.
}

\examples{

flist <- freqlist(tokenize("the old man and the sea."))
summary(flist) 
print(summary(flist))

(flist_sum <- summary(flist))
names(flist_sum)
flist_sum[["n_tokens"]]
flist_sum$n_tokens
flist_sum[["n_types"]]
flist_sum$n_types

flist2 <- flist["man", invert = TRUE]
(flist2_sum <- summary(flist2))
names(flist2_sum)
flist2_sum[["n_tokens"]]
flist2_sum$n_tokens
flist2_sum[["n_types"]]
flist2_sum$n_types
flist2_sum[["tot_n_tokens"]]
flist2_sum$tot_n_tokens


}

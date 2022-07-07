\name{sort.freqlist}
\alias{sort.freqlist}
\title{
Sort a Frequency List
}
\description{
Sort an object of the class \code{'freqlist'}.
}
\usage{

\method{sort}{freqlist}(x,
     decreasing = FALSE,
     sort_crit = c("ranks", "names", "orig_ranks", "freqs"),
     na_last = TRUE,
     \dots)
}
\arguments{
  \item{x}{
   an object of the class \code{'freqlist'}.
  }
  \item{decreasing}{
   logical value that indicates whether the items should be
   sorted from small to large (when \code{decreasing} in
   \code{FALSE}) or from large to small
   (when \code{decreasing} is \code{TRUE}). The default
   value is \code{FALSE}. Note, however that ranking in frequency
   lists is such that
   lower ranks correspond to higher frequencies. Therefore,
   sorting by rank (either 
    \code{"ranks"} or \code{"orig_ranks"}) with \code{decreasing} set
    to its default value \code{FALSE} results
   in the highest frequencies ending up at the beginning of the sorted
   list.
  }
  \item{sort_crit}{
   character value that determines the sorting criterion. If
   \code{sort_crit} is \code{ranks}, then the items in the frequency
   list are sorted by their current frequency rank. 
   \code{sort_crit} is \code{names}, then the items in the frequency
   list are sorted alphabetically their name. 
   If
   \code{sort_crit} is \code{orig_ranks}, then the items in the frequency
   list are sorted by their original ranks (if those are present), or by their
   current frequency ranks (if no original ranks are present).
   Finally, sorting with
   \code{sort_crit} set to \code{freqs} is identical to sorting by frequency
   ranks, but with the meaning of the argument \code{decreasing} being reversed.
   In other words,
   sorting by frequencies (\code{"freqs"}) with \code{decreasing} set
    to its default value \code{FALSE} results
   in the lowest frequencies ending up at the beginning of the sorted
   list.
  }
  \item{na_last}{
   logical value of length one that determines how \code{NA} values
   should be sorted. This argument is only relevant when
   \code{sort_crit} is \code{"orig_ranks"} because currently since names and
   frequencies are not allowed to be \code{NA} in frequency lists.
   If \code{na_last} is \code{TRUE}, then items with a sorting criterion of
   \code{NA} end up at the end of the sorted frequency list. If \code{na_last}
   is \code{FALSE}, then items with a sorting criterion of \code{NA} end up at
   the start of the sorted frequency list. If \code{na_last} is \code{NA},
   then items with a sorting criterion of \code{NA} are removed from the
   sorted frequency list.
  }
  \item{\dots}{
   additional arguments.
  }
}
\details{
  Because of the way ranks are calculated for ties (with lower ranks being
  assigned to
  ties earlier in the list), sorting the list may affect the ranks of ties.
  More specifically, ranks among
  ties may differ depending on the criterion that is used to sort the
  frequency list.
}
\value{
The function described on this page returns an object of the class
  \code{'freqlist'}.
}

\examples{

(flist <- freqlist(tokenize("the old story of the old man and the sea.")))

sort(flist)
sort(flist, decreasing = TRUE)

}

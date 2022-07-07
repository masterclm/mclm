\name{as_tibble.assoc_scores}
\alias{as_tibble.assoc_scores}
\title{
Coerce association scores to tibble
}
\description{
Coerces an oject of the class \code{'assoc_scores'} to a tibble
}
\usage{
\method{as_tibble}{assoc_scores}(x, \dots)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{'assoc_scores'}.
  }
  \item{\dots}{
   other arguments.
  }
}
\value{
This function returns a tibble.
}
\examples{
txt1 <- "we're just two lost souls swimming in a fish bowl,
         year after year, running over the same old ground,
         what have we found? the same old fears.
         wish you were here."
flist1 <- freqlist(txt1, as_text = TRUE)
txt2 <- "picture yourself in a boat on a river
         with tangerine dreams and marmelade skies
         somebody calls you, you answer quite slowly
         a girl with kaleidoscope eyes"
flist2 <- freqlist(txt2, as_text = TRUE)
(scores <- assoc_scores(flist1, flist2, min_freq = 0))

as_tibble(scores)


}

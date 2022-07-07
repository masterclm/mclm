\name{merge_conc}
\alias{merge_conc}
\title{
Merges several concordances into one large concordance
}
\description{
  Merges several concordances into one large concordance 
}
\usage{
merge_conc(...,
           show_warnings = TRUE) 
}
\arguments{
  \item{...}{
   two or more arguments that represent concordances
  }
  \item{show_warnings}{
  boolean value; if set to \code{FALSE}, then warnings are suppressed.
  }
}
\value{
  The function \code{merge_conc} returns an object of the class \code{conc}.
}
\examples{
(cd_1 <- conc('A first very small corpus.', '\\\\w+', as_text = TRUE))
as.data.frame(cd_1)
(cd_2 <- conc('A second very small corpus.', '\\\\w+', as_text = TRUE))
(cd_3 <- conc('A third very small corpus.', '\\\\w+', as_text = TRUE))
(cd <- merge_conc(cd_1, cd_2, cd_3))
as.data.frame(cd)
}

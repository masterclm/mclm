\name{conc_to_dataset_bibleworks}
\alias{conc_to_dataset_bibleworks}
%- Also NEED an '\alias' for EACH other topic documented here
\title{
Convert concordances generated with BibleWorks to a dataset
}
\description{
  Reads one or several concordances generated with BibleWorks, and
  converts their contents to a single dataset that can be read
  with the function \code{\link{read_dataset}}.
  The encoding in the input file(s) is assumed to be 'UTF-8'.
   The encoding in the output is 'UTF-8'.
}
\usage{
conc_to_dataset_bibleworks(x, metafiles, outfile)
}
\arguments{
  \item{x}{
   name of the input file(s) with the actual concordance(s).
   This can be either a single name, or
   a longer vector with several names. In the latter case, the
   contents of all input files is merged before writing the
   output file.
  }
  \item{metafiles}{
   name of the input file(s) with the metadata.
   This can be either a single name, or
   a longer vector with several names. There should be as many
   items in \code{metafiles} as there are in \code{x}
   and they should be aligned in such a way that the i-th
   item in \code{metafiles} corresponds to the i-th item
   in \code{x}.
  }
  \item{outfile}{
   Name of the output file. The encoding in the output is
   'UTF-8'.
  }
}
\value{
  This function invisibly returns the value of its first argument
  \code{x}.
}

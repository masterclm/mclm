\name{get_fnames}
\alias{get_fnames}
\title{
Retrieve the names of the files in a give path
}
\description{
Build an object of the class \code{'fnames'}.
}
\usage{
get_fnames(path = ".",
           re_pattern = NULL,
           recursive = TRUE,
           perl = TRUE,
           invert = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
   the location of the files the names of which need to be listed.
  
  }
  \item{re_pattern}{
   Optional regular expression; if present, then only the filenames
   are kept which have a match for \code{re_pattern}. But also see
   the argument \code{invert}. Notice that the match for the regular
   expression is not searched in the actual filename only, but in 
   its long filename (including the path leading to the file).
  }
  \item{recursive}{
   Boolean value which indicates whether subdirectories of 
   \code{path} should also be searched.
  }
  \item{perl}{
   Boolean value indicating whether or not \code{re_pattern} should
   be interpreted as a Perl flavor regular expression.

  }  
  \item{invert}{
   Boolean value that can change the way \code{re_pattern} is
   used. If \code{invert} is \code{TRUE}, then the filenames 
   are selected which do NOT contain a match for \code{re_pattern}.
  }
}
\value{
This function returns an object of the
  class \code{'fnames'}. The result does not merely 
  contain the actual filenames, but instead contains 
   the long filenames (including the path leading to the file).
}
\examples{
\dontrun{
cwd_fnames <- get_fnames(recursive = FALSE)
cwd_fnames
summary(cwd_fnames)
}
         
}

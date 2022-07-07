\name{find_xpath}
\alias{find_xpath}
\title{
Find matches for an XPath query in a corpus
}
\description{
  Find matches for an XPath query in a corpus
}
\usage{
find_xpath(pattern,
           x,
           handlers = NULL,
           trim = TRUE,
           fun = NULL,
           final_fun = NULL,
           namespaces = NULL,
           ...) 
}
\arguments{
  \item{pattern}{
   an XPath query
  }
  \item{x}{
   a corpus
  }
  \item{handlers}{
   Optional collection of functions used to map the different XML nodes to
   R objects.
  }
  \item{trim}{
   whether to strip white space from the beginning and end of text strings.
  }
  \item{fun}{
   optional function that is to be applied to the individual matches prior
   to returning the result.
  }
  \item{final_fun}{
   optional final function that is to be applied to the complete list of
   matches prior to returning the result.
  }
  \item{namespaces}{
   optional namespaces.
  }
  \item{...}{
   optional additional arguments.
  }
}
\value{
  Returns an object of the class .
}

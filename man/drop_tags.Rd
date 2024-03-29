% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{drop_tags}
\alias{drop_tags}
\title{Drop XML tags from character string}
\usage{
drop_tags(x, half_tags_too = TRUE)
}
\arguments{
\item{x}{String with XML tag}

\item{half_tags_too}{Logical. Whether tags with only opening/closing
bracket should also be removed.}
}
\value{
Character string
}
\description{
This function takes a character vector and returns a copy from which all
XML-like tags have been removed. Moreover, if \code{half_tags_too = TRUE}
any half tag at the beginning or end of \code{x} is also removed.
}
\details{
This function is not XML-aware. It uses a very simple definition of what
counts as a tag. More specifically, any character sequence starting with
\code{<} and ending with \code{>} is considered a 'tag'; inside such a tag, between
\code{<} and \code{>}, \code{drop_tags()} accepts any sequence of zero or more characters.
}
\examples{
xml_snippet <- "id='3'/><w pos='Det'>An</w> <w pos='N'>example</w> <w"
drop_tags(xml_snippet)
drop_tags(xml_snippet, half_tags_too = FALSE)
}

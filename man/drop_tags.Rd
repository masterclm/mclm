\name{drop_tags}
\alias{drop_tags}
\title{
Drop all XML tags, or XML-like tags, from a character vector
}
\description{
The function \code{drop.tags} takes as its input a character vector
 \code{x}, and returns a copy of \code{x} from which all XML-like tags
  have been removed.  Moreover, in case of the setting 
 \code{half_tags_too = TRUE}, any half tag at the beginning or the
 end of \code{x}
 is also remove.
 
 This function is not truly XML-aware. It uses a very simple definition of
 what is a `tag'. More specifically, any character sequence starting with
 \code{<} and ending
 with \code{>} is considered a `tag'. Inside such a the tag, between
 \code{<} and \code{>},
 this function accepts any sequence of zero or more characters; all characters
 are accepted inside a tag, with the exception of \code{>}.
}
\usage{
drop_tags(x,
          half_tags_too = TRUE) 

}
\arguments{
  \item{x}{
  the argument \code{x} contains the character vector from which the
  tags need to
  be removed.
}
  \item{half_tags_too}{
  in case of the setting \code{half_tags_too = TRUE}, half tags at the
  beginning or the
  end of \code{x} are also removed; in case of the setting
  \code{half_tags_too = FALSE}, only
  complete tags are removed.
}
}
\value{
Returns a character vector that is a copy of \code{x} from which all tags have been
 removed.
}
\examples{
xml_snippet <- "id='3'/><w pos='Det'>An</w> <w pos='N'>example</w> <w"
drop_tags(xml_snippet)
drop_tags(xml_snippet, half_tags_too = FALSE)

}

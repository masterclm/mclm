\name{re_retrieve_all}
\alias{re_retrieve_first}
\alias{re_retrieve_last}
\alias{re_retrieve_all}
\alias{re_has_matches}
\alias{re_which}
\alias{re_replace_first}
\alias{re_replace_all}
\title{
Convenience Functions in Support of Regular Expressions
}
\description{
The functions \code{re_retrieve_first}, \code{re_retrieve_last},
\code{re_retrieve_all}, \code{re_has_matches}, \code{re_which},
\code{re_replace_first}, and \code{re_replace_all} ar convenience
functions in support of regular expressions. The essentially are
simple wrappers around the \code{base} function as
\code{\link{regexpr}}, \code{\link{gregexpr}},
\code{\link{grepl}}, \code{\link{grep}},
\code{\link{sub}}, and \code{\link{gsub}}.
The most important differences between the functions documented
here and those \code{base} functions is the order of the arguments
(\code{x} before \code{pattern}) 
and the fact that the argument \code{perl} is set to \code{TRUE}
by default.
}
\usage{
re_retrieve_first(x, pattern, 
                  ignore.case = FALSE, perl = TRUE,
                  fixed = FALSE, useBytes = FALSE, 
                  requested_group = NULL,
                  drop_NA = FALSE,
                  \dots)

re_retrieve_last(x, pattern, 
                 ignore.case = FALSE, perl = TRUE,
                 fixed = FALSE, useBytes = FALSE, 
                 requested_group = NULL,
                 drop_NA = FALSE,
                 \dots)

re_retrieve_all(x, pattern, 
                ignore.case = FALSE, perl = TRUE,
                fixed = FALSE, useBytes = FALSE, 
                requested_group = NULL,
                unlist = TRUE,
                \dots)

re_has_matches(x, pattern, 
               ignore.case = FALSE, perl = TRUE,
               fixed = FALSE, useBytes = FALSE, 
               \dots)
               
re_which(x, pattern, 
         ignore.case = FALSE, perl = TRUE,
         fixed = FALSE, useBytes = FALSE, 
         \dots)

re_replace_first(x, pattern, replacement,
                 ignore.case = FALSE, perl = TRUE,
                 fixed = FALSE, useBytes = FALSE, 
                 \dots)

re_replace_all(x, pattern, replacement,
               ignore.case = FALSE, perl = TRUE,
               fixed = FALSE, useBytes = FALSE, 
               \dots)

}
\arguments{
  \item{x}{
the character data that are to be searched or modified
}
  \item{pattern}{
regular expression that specifies what is to be searched
}
  \item{replacement}{
length one character vector that specifies the replacement string;
it is to be taken literally, except that the notation
\code{\\1} or \code{\\2} etc. can be used to refer to groups in
\code{pattern}.
}
  \item{ignore.case}{
this argument has the same meaning it also has in \code{base} functions such as \code{\link{grep}}.
}
  \item{perl}{
boolean value that indicates whether or not rhe Perl flavor of regular
expressions is to be used. Its default value is \code{TRUE}, which is
different from its default value in \code{base} functions such as \code{\link{grep}}.
}
  \item{fixed}{
this argument has the same meaning it also has in \code{base} functions such as \code{\link{grep}}.
}
  \item{useBytes}{
this argument has the same meaning it also has in \code{base} functions such as \code{\link{grep}}.
}
  \item{drop_NA}{
if the argument \code{drop_NA} is set to \code{FALSE}, then the result of the function always has the same length as the input argument \code{x}, and items in \code{x} that don't contain a match for \code{pattern} yield an \code{NA} in the output. If \code{drop_NA} is set to \code{TRUE}, those \code{NA} values are removed from the result and therefore the result may contain fewer items than \code{x}.
}
  \item{unlist}{
if the argument \code{unlist} is set to \code{FALSE}, then the result of the function always has the same length as the input argument \code{x}. More specifically, the result will be a list, and in that list, input items from \code{x} that don't contain a match for \code{pattern} yield an empty vector and input items that do contain matches will yield a vector of length one or more (depending on the number of matches). If \code{unlist} is set to \code{TRUE}, the result is a single vector the length of which may be shorter or longer than \code{x}.
}
  \item{requested_group}{
  If \code{requested_group} is \code{NULL} or \code{0}, then the result will contain matches for \code{pattern} as a whole. If \code{requested_group} is \code{1}, then the result will not contain matches for \code{pattern} but instead will containt matches for the first capturing group in \code{pattern}. Likewise, if \code{requested_group} is \code{2}, the output will contain matches for the second capturing group. Etc.
}
  \item{\dots}{
additional arguments.
}
}
\details{
  The functions \code{re_retrieve_first} retrieves from each item in
\code{x} the first match for \code{pattern}. The function
\code{re_retrieve_last} does the same for the last match in each
item of \code{x}. The function \code{re_retrieve_all} retrieves
all matches in all items in \code{x}. Under the hood the three
functions use the \code{base} function \code{\link{gregexpr}}.

The function \code{re_has_matches} is a simple wrapper around
the \code{base} function \code{\link{grepl}}. The function \code{re_which} is a simple wrapper around
the \code{base} function \code{\link{grep}}. 

The function \code{re_replace_first} is a simple wrapper around
the \code{base} function \code{\link{sub}}. The function 
\code{re_replace_all} is a simple wrapper around
the \code{base} function \code{\link{gsub}}. 

}
\value{
  The functions \code{re_retrieve_first},
\code{re_retrieve_last}, and \code{re_retrieve_all} 
return either a single vector of character data as \code{x} or a list containing such vectors.

The function \code{re_has_matches} returns a boolean vector.
The function \code{re_which} contains a numeric vector. 

The functions \code{re_replace_first} and
\code{re_replace_all} return the same type of character data
as \code{x}. 
}
\examples{
x <- tokenize("This is a sentence with a couple of words in it.")
pattern <- "[oe].(.)"

re_retrieve_first(x, pattern)
re_retrieve_first(x, pattern, drop_NA = TRUE)
re_retrieve_first(x, pattern, requested_group = 1)
re_retrieve_first(x, pattern, drop_NA = TRUE, requested_group = 1)

re_retrieve_last(x, pattern)
re_retrieve_last(x, pattern, drop_NA = TRUE)
re_retrieve_last(x, pattern, requested_group = 1)
re_retrieve_last(x, pattern, drop_NA = TRUE, requested_group = 1)

re_retrieve_all(x, pattern)
re_retrieve_all(x, pattern, unlist = FALSE)
re_retrieve_all(x, pattern, requested_group = 1)
re_retrieve_all(x, pattern, unlist = FALSE, requested_group = 1)

re_replace_first(x, "([oe].)", "{\\\\1}")
re_replace_all(x, "([oe].)", "{\\\\1}")

}

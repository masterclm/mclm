\name{tokenize}
\alias{tokenize}
\title{
Regular expression based tokenization
}
\description{
Splits a text into a sequence of tokens, using regular expressions to
identify tokens. Returns an object of the class \code{"tokens"}.
}
\usage{
tokenize(x,
         re_drop_line = NULL,
         line_glue = NULL,
         re_cut_area = NULL,
         re_token_splitter = re("[^_\\\\p{L}\\\\p{N}\\\\p{M}'-]+"),
         re_token_extractor = re("[_\\\\p{L}\\\\p{N}\\\\p{M}'-]+"),
         re_drop_token = NULL,
         re_token_transf_in = NULL,
         token_transf_out = NULL,
         token_to_lower = TRUE,
         perl = TRUE,
         ngram_size = NULL,
         max_skip = 0,
         ngram_sep = "_",
         ngram_n_open = 0,
         ngram_open = "[]")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   the object \code{x} either is a character vector or
   an object of the class \code{"TextDocument"} and contains the
   text that is to be tokenized.
   
   If \code{x} is a character vector and the length of \code{x}
   is higher than one, then each item in \code{x} is treated as a separate
   line (or a separate series of lines) in the text. Withing each
   item of \code{x}, the character \code{"\\\\n"} is also treated as
   a line separator.
  }
  \item{re_drop_line}{
   if \code{re_drop_line} is \code{NULL}, then this argument is ignored.
   Otherwise, \code{re_drop_line} is a character vector (assumed to
   be of length 1) containing a regular expression. Lines in \code{x}
   that contain a match for \code{re_drop_line} are
   treated as not belonging to the corpus and are excluded from
   the results.
  }
  \item{line_glue}{
   if \code{line_glue} is \code{NULL}, then this argument is ignored.
   Otherwise, all lines in a corpus file (or in \code{x}, if
   \code{as_text} is \code{TRUE}, are glued together in one
   character vector of length 1, with the string \code{line_glue}
   pasted in between consecutive lines.  The value of \code{line_glue}
   can also be equal to the empty string \code{""}.
   The `line glue' operation is conducted immediately after the `drop line'
   operation.
  }
  \item{re_cut_area}{
   if \code{re_cut_area} is \code{NULL}, then this argument is ignored.
   Otherwise, all matches in a corpus file (or in \code{x}, if
   \code{as_text} is \code{TRUE}, are 'cut out' of the text prior
   to the identification of the tokens in the text (and are
   therefore not taken into account when identifying the tokens). 
   The `cut area' operation is conducted immediately after the
   `line glue'
   operation.

  }  
  \item{re_token_splitter}{
   the actual token identification is either based on
   \code{re_token_splitter}, a regular expression that identifies the
   areas between the tokens, or on \code{re_token_extractor}, a regular
   expressions that identifies the area that are the tokens. The first
   mechanism is the default mechanism: the 
   argument  \code{re_token_extractor} is only used
   if \code{re_token_splitter} is \code{NULL}.
   
   more specifically, \code{re_token_splitter} is a regular expression
   that identifies the
   locations where lines in the
   corpus files are split into
   tokens. 
   The `token identification' operation is conducted immediately after the
   `cut area'
   operation.
   
  }
  \item{re_token_extractor}{
   a regular expression that identifies the locations of the
   actual tokens. This
   argument is only used
   if \code{re_token_splitter} is \code{NULL}. Whereas matches for
   \code{re_token_splitter} are
   identified as the areas between the tokens, matches for
   \code{re_token_extractor} are
   identified as the areas of the actual tokens. Currently the
   implementation of
   \code{re_token_extractor} is a lot less time-efficient
   than that of \code{re_token_splitter}.
   The `token identification' operation is conducted immediately after the
   `cut area'
   operation.
  }
  \item{re_drop_token}{
   a regular expression that identifies tokens that are to be excluded
   from the results. Any token that contains a match for
   \code{re_drop_token} is removed from the results. If
   \code{re_drop_token} is \code{NULL}, this argument is ignored. 
   The `drop token' operation is conducted immediately after the
   `token identification'
   operation.
   
  }
  \item{re_token_transf_in}{
   a regular expression that identifies areas in the tokens that are to be
   transformed. This argument works together with the argument
   \code{token_transf_out}.
   
   If both \code{re_token_transf_in} and \code{token_transf_out} differ
   from \code{NULL}, then all matches, in the tokens, for the
   regular expression  \code{re_token_transf_in} are replaced with
   the replacement string \code{token_transf_out}.

   The `token transformation' operation is conducted immediately after the
   `drop token'
   operation.

  }
  \item{token_transf_out}{
   a `replacement string'. This argument works together with
   \code{re_token_transf_in} and is ignored if
   \code{re_token_transf_in} is \code{NULL}.
   
  }
  \item{token_to_lower}{
    a boolean value that determines whether or not tokens must be converted
    to lowercase before returning the result.
    
   The `token to lower' operation is conducted immediately after the
   `token transformation'
   operation.
    
  }
  \item{perl}{
    Boolean value that determines whether or not the PCRE regular expression
    flavor is being used in the arguments that contain regular expressions.
  }
  \item{ngram_size}{
    Argument in support of ngrams/skipgrams (also see \code{max_skip}).
    
    If one wants to identify individual tokens, the value of \code{ngram_size}
    should be \code{NULL} or \code{1}. If one wants to retrieve
    token ngrams/skipgrams, \code{ngram_size} should be an integer indicating
    the size of the ngrams/skipgrams. E.g. \code{2} for bigrams, or \code{3} for
    trigrams, etc.
  }
  \item{max_skip}{
    Argument in support of skipgrams. This argument is ignored if
    \code{ngram_size} is \code{NULL} or is \code{1}.
  
    If \code{ngram_size} is \code{2} or higher, and \code{max_skip}
    is \code{0}, then regular ngrams are being retrieved (albeit that they
    may contain open slots; see \code{ngram_n_open}).
    
    If \code{ngram_size} is \code{2} or higher, and \code{max_skip}
    is \code{1} or higher, then skipgrams are being retrieved (which in the
    current implementation cannot contain open slots; see \code{ngram_n_open}).
 
    For instance, if \code{ngram_size} is \code{3} and \code{max_skip} is
    \code{2}, then 2-skip trigrams are being retrieved.
    Or if \code{ngram_size} is \code{5} and \code{max_skip} is
    \code{3}, then 3-skip 5-grams are being retrieved.
    
  }
  \item{ngram_sep}{
    Length one character vector containing the string that is used to
    separate/link tokens in the representation of ngrams/skipgrams
    in the output of
    this function.
  }  
  \item{ngram_n_open}{
    If \code{ngram_size} is \code{2} or higher, and moreover
    \code{ngram_n_open} is a number higher than \code{0}, then
    ngrams with `open slots' in them are retrieved. These
    ngrams with `open slots' are generalisations of fully lexically specific
    ngrams (with the generalisation being that one or more of
    the items
    in the ngram are replaced by a notation that stands for
    `any arbitrary token').
    For instance, if \code{ngram_size} is \code{4} and \code{ngram_n_open} is
    \code{1}, and if moreover the input contains a
    4-gram \code{"it_is_widely_accepted"}, then the output will contain
    all modifications of
    \code{"it_is_widely_accepted"} in which one (since
    \code{ngram_n_open} is \code{1}) of the items in this n-gram is
    replaced by an open slot. The first and the last item inside
    an ngram are never turned into an open slot; only the items in between
    are candidates for being turned into open slots. Therefore, in the
    example, the output will contain \code{"it_[]_widely_accepted"} and
    \code{"it_is_[]_accepted"}. 
    As a second example, if \code{ngram_size} is \code{5} and
    \code{ngram_n_open} is \code{2}, and if moreover the input contains a
    5-gram \code{"it_is_widely_accepted_that"}, then the output will contain
    \code{"it_[]_[]_accepted_that"}, \code{"it_[]_widely_[]_that"}, and
    \code{"it_is_[]_[]_that"}.
  }  
  \item{ngram_open}{
    String that is used to represent open slots in ngrams in the
    output of this function.
  }  
}
\details{
  If the output of this function contains ngrams with open slots, then
  the order of the items in the output is no longer meaningful.
  For instance,  if \code{ngram_size} is \code{5} and
    \code{ngram_n_open} is \code{2}, and if moreover the input contains a
    5-gram \code{"it_is_widely_accepted_that"}, then the output will contain
    \code{"it_[]_[]_accepted_that"}, \code{"it_[]_widely_[]_that"}, and
    \code{"it_is_[]_[]_that"}. However, the relative order of these three
    items in
    the output must be considered to be arbitrary.
}
\value{
This function returns a token sequence, i.e. an object of the
  class \code{"tokens"}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
tks <- tokenize(toy_corpus)
print(tks, n = 1000)
tks <- tokenize(toy_corpus, re_token_splitter = "\\\\W+")
print(tks, n = 1000)

tokenize(toy_corpus, ngram_size = 3)

tokenize(toy_corpus, ngram_size = 3, max_skip = 2)

tokenize(toy_corpus, ngram_size = 3, ngram_n_open = 1)

}

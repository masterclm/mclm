\name{types}
\alias{types}
\title{
Build a 'types' Object
}
\description{
Build an object of the class \code{'types'}.
}
\usage{
types(x,
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
      blocksize = 300,
      verbose = FALSE,
      show_dots = FALSE,
      dot_blocksize = 10,
      file_encoding = "UTF-8",
      ngram_size = NULL,
      ngram_sep = "_",
      ngram_n_open = 0,
      ngram_open = "[]",
      as_text = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   the object \code{x} either contains the list of filenames of the
   corpus files (if \code{as_text} is \code{TRUE}) or the actual text
   of the corpus (if \code{as_text} is \code{FALSE}).
   
   If (if \code{as_text} is \code{TRUE}) and the length of the vector \code{x}
   is higher than one, then each item in \code{x} is treated as a separate
   line (or a separate series of lines) in the corpus text. Withing each
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
   from \code{NA}, then all matches, in the tokens, for the
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
    a boolean value that determines whether or not the PCRE regular expression
    flavor is being used in the arguments that contain regular expressions.
  }
  \item{blocksize}{
    number that indicates how many corpus files are read to memory
    `at each individual step' during the steps in the procedure;
    normally the default value
    of \code{300} should not
    be changed, but when one works with exceptionally small corpus files,
    it may be worthwhile to use a higher number, and when one works with
    exceptionally large corpus files, ot may be worthwhile to use a
    lower number.
  }
  \item{verbose}{
    if \code{verbose} is \code{TRUE}, messages are printed to the console to
    indicate progress.
  }
  \item{show_dots}{
    if \code{verbose} is \code{TRUE}, dots are printed to the console to
    indicate progress.
  }
  \item{dot_blocksize}{
    if \code{verbose} is \code{TRUE}, dots are printed to the console to
    indicate progress.
  }
  \item{file_encoding}{
  file encoding that is assumed in the corpus files.
  }
  \item{ngram_size}{
    for a regular frequency list, i.e. a frequency list based on
    individual tokens, the value of \code{ngram_size} should be
    \code{NULL}; for a frequency list of ngrams of tokens,
    \code{ngram_size} should be a single number indicating the size of
    the ngrams. E.g. \code{2} for bigrams, or \code{3} for trigrams, etc.
  }
  \item{ngram_sep}{
    Length one character vector containing the string that is used to
    separate tokens in the representation of ngrams.
  }
 \item{ngram_n_open}{
    If \code{ngram_size} is some number higher than \code{1}, and moreover
    \code{ngram_n_open} is a number higher than \code{0}, then
    \code{freqlist()} works with n-grams with `open slots' in them. These
    n-grams with `open slots' are generalisations of fully lexically specific
    n-grams (with the generalisation being that one or more of the items
    in the n-gram are replaced by a notation that stands for
    `any arbitrary token').
    For instance, if \code{ngram_size} is \code{4} and \code{ngram_n_open} is
    \code{1}, and if moreover the corpora contains a
    4-gram \code{"it is widely accepted"}, then \code{freqlist()} will work with
    all modifications of
    \code{"it is widely accepted"} in which one (since
    \code{ngram_n_open} is \code{1}) of the items in the n-gram is
    replaced by an open slot. The first and the last item in
    an n-gram are never turned into an open slot; only the items in between
    are candidates for being turned into open slots. Therefore, in the
    example, the output will work with the n-grams 
    \code{"it [] widely accepted"} and
    \code{"it is [] accepted"}. 
    As a second example, if if \code{ngram_size} is \code{5} and
    \code{ngram_n_open} is \code{2}, and if moreover the corpora contain a
    5-gram \code{"it is widely accepted that"}, then \code{freqlist()} will work
    with
    \code{"it [] [] accepted that"}, \code{"it [] widely [] that"}, and
    \code{"it is [] [] that"}.
  }  
  \item{ngram_open}{
    String that is used to represent open slots in n-grams.
  }  
  \item{as_text}{
  boolean vector, assumed to be of length 1, which determines whether
  \code{x} is to be interpreted as a character vector containing the
  actual contents of the corpus
  (if \code{as_text} is \code{TRUE}) or as a character vector containing the
  names of the corpus files (if \code{as_text} is \code{FALSE}).
  If if \code{as_text} is \code{TRUE}, then the arguments
  \code{blocksize}, \code{verbose}, \code{show_dots}, \code{dot_blocksize},
  and \code{file_encoding} are ignored. 
  }
}
\value{
This function returns an object of the
  class \code{'types'}. The items in the output are sorted by their
  frequency in the input.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
types(toy_corpus, as_text = TRUE)
         
}

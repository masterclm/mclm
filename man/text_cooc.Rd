\name{text_cooc}
\alias{text_cooc}
\title{
Build textual collocation frequencies.
}
\description{
Builds textual collocation frequencies for a specific node.
}
\usage{
text_cooc(x, 
          re_node,
          re_boundary = NULL,
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
          dot_blocksize = 10,
          file_encoding = "UTF-8")
}
\arguments{
  \item{x}{
   the object \code{x} contains the list of filenames of the
   corpus files.
  }
  \item{re_node}{
   regular expression used for identifying instances of the
   `node', i.e. the target item, for which surface collocation
   information is collected. Any token that contains a match for
   \code{re_node} is considered to be an instance of the `node'.
  }    
  \item{re_boundary}{
   regular expression used for identifying boundaries between
   `textual units'. Any token that contains a match for
   \code{re_boundary} is considered to be a boundary in between
   two `textual units'.
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
  \item{dot_blocksize}{
    if \code{verbose} is \code{TRUE}, dots are printed to the console to
    indicate progress.
  }
  \item{file_encoding}{
  each corpus file is interpreted as a text file the encoding of which is
  the one given in \code{file_encoding}. 
  The
  argument \code{file_encoding} can either be a character vector of length one,
  or
  a character vector with the same length as \code{x}. In the former case, all
  files in \code{x} are assumed to have the same encoding. In the latter
  case, different
  files can have different encodings.
  }
}
\details{
Two major steps can be distinguished in the procedure conducted by
  \code{surf_cooc}. The first major step is the \emph{identification of
  the (sequence of) tokens} that, for the purpose of this analysis,
  will be considered to be the content of the corpus.
  The function arguments that jointly determine the details of
  this step are 
  \code{re_drop_line}, \code{line_glue}, \code{re_cut_area},
  \code{re_token_splitter}, \code{re_token_extractor},
  \code{re_drop_token}, \code{re_token_transf_in},
  \code{token_transf_out}, and \code{token_to_lower}.
  The sequence of tokens that is the ultimate outcome of this step
  is then handed over to the second major step of the procedure.
  
  The second major step is the \emph{establishment of the
  co-occurrence frequencies}. The function arguments
  that jointly determine the details of this step are
  \code{re_node} and
  \code{re_boundary}. It is important to know that this
  second step is conducted after the tokens of the corpus have
  been identified, and that it is applied to a sequence of
  tokens, not to the original text. More specifically the
  regular expressions \code{re_node} and \code{re_boundary}
  are tested against individual tokens, as they are identified
  by the token identification procedure. 
}
\value{
The function \code{text_cooc} returns an object of the class
  \code{"cooc_info"}, containing information on co-occurrence
  frequencies.
}

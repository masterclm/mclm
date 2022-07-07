\name{slma}
\alias{slma}
\title{
Conduct a stable lexical marker analysis
}
\description{
Conducts a stable lexical marker analysis.
}
\usage{
slma(x,
     y,
     file_encoding = "UTF-8",
     sig_cutoff = qchisq(.95, df = 1),
     small_pos = 0.00001,
     keep_intermediate = FALSE,
     verbose = TRUE,
     min_rank = 1,
     max_rank = 5000,
     keeplist = NULL,
     stoplist = NULL,
     ngram_size = NULL,
     max_skip = 0,
     ngram_sep = "_",
     ngram_n_open = 0,
     ngram_open = "[]",
     ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A character vector containing the filenames of the `A set' of documents.
  }
  \item{y}{
    A character vector containing the filenames of the `B set' of documents.
  }
  \item{file_encoding}{
   if either \code{corp_a} or \code{corp_b} is a folder name, or both are
   folder names, then all the files
   in these folders are assumed to be text files encoded with
   \code{file_encoding}.  
  }
  \item{sig_cutoff}{
   numerical argument that indicates the cutoff value for
   `significance' in the stable lexical marker analysis. The
   default value of \code{sig_cutoff}
   is \code{qchisq(.95, df = 1)}, which is about \code{3.841}.
  }
  \item{small_pos}{
   small positive number that is used instead of zero for all zero frequencies
   all keyword analyses, in order to avoid situations where the
   calculations would otherwise throw errors or return
   (minus) infinity values.
  }

  \item{keep_intermediate}{
   Boolean value that indicates whether or not intermediate calculation
   results are kept in the output. If you want to be able to
   inspect details of the
   results with the function \code{\link{details.slma}}, then you should
   make sure that
   \code{keep_intermediate} is \code{TRUE}.
  }
  \item{verbose}{
    Boolean value that indicates whether or not progress is printed to
    the console during the analysis.
  }
  \item{min_rank}{
    Only tokens (or token n-grams) with a frequency rank greater than or
    equal to \code{min_rank} in the A-corpus (as a whole) are taken into
    consideration as candidate stable markers.
  }
  \item{max_rank}{
    Only tokens (or token n-grams) with a frequency rank smaller than or
    equal to \code{max_rank} in the A-corpus (as a whole) are taken into
    consideration as candidate stable markers.
  }
  \item{keeplist}{
    List of types that must certainly be included in the list of candidate
    markers (disregarding the \code{max_rank} criterion and disregarding the
    \code{droplist}).
  }
  \item{stoplist}{
    List of types that must not  be included in the list of candidate
    markers (disregarding the \code{max_rank} criterion). Note, however,
    that in case of conflicting arguments, the \code{keeplist} arguments
    takes precedence of the \code{droplist} arguments. In other words,
    types that are included in both lists will be kept as candidate
    markers.
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
  \item{...}{
    Any additional arguments.
  }                  
  
}
\details{
A stable lexical marker analysis (SLMA) of the A-documents in
 \code{corp_a} versus the B-documents in \code{corp_b}
 starts from separate keyword analyses for all possible
 document couples
 \code{(a,b)}, with \code{a} an A-document and \code{b} a
 B document. If there are \var{n} A-documents and \var{m}
 B-documents, then
 \eqn{n * m} keyword analyses are conducted. The 'stability'
 of a linguistic item \code{x}, as a marker for the collection
 of A-documents (when compared to the B-documents), 
 corresponds to the frequency and consistency with which \code{x}
 is found to be a keyword for the A-document across all
 aforementioned keyword analyses.
 
 In any specific keyword analysis, item \code{x} is considered
 a keyword for the A-document, if \code{G_signed}
  is positive and moreover
 \code{p_G} is less than \code{sig_cutoff}.
 (See \code{\link{assoc_scores}} for more information on
 \code{G_signed} and \code{p_G}). Item \code{x} is considered
 a keyword for the B-document, if \code{G_signed}
  is negative and moreover
 \code{p_G} is less than \code{sig_cutoff}.
 
}
\value{
This function returns an object of the class \code{"slma"}.
 
 Rows in the returned object are linguistics items (these items also
 feature as row names), and colums are different
 'stability measures' and related statistics. By default, the linguistic items are
 sorted by decreasing 'stability', according to the \code{S_lor}
 'stability measure'.

The following are columns in the output:

  \item{S_abs }{The 'absolute stability' of the linguistic item, calculated
                as \code{S_att} minus \code{S_rep}. The
                maximum possible value of \code{S_abs} is \eqn{n * m}, with
                \var{n} the number of A-documents and \var{m} the number of
                B-documents. The minimum possible value of \code{S_abs} is
                minus \eqn{n * m}.}
  \item{S_nrm }{The 'normalized stability' of the linguistic item, calculated
                as \code{S_abs} divided by \eqn{n * m}. Possible values for
                \code{S_nrm} range from minus one to plus one.}
  \item{S_att }{The 'stability of attraction', which is the absolute number
                of cases, i.e. number of \code{(a,b)} couples, in which the
                linguistic item is a keyword for the A-document. Possible
                values range from zero to \eqn{n * m}.}
  \item{S_rep }{The 'stability of repulsion', which is the absolute number
                of cases, i.e. number of \code{(a,b)} couples, in which the
                linguistic item is a keyword for the B-document. Possible
                values range from zero to \eqn{n * m}.}
  \item{lor_min }{The lowest attested \code{log_OR} measure across all
                  \code{(a,b)} couples for which \code{p_G} is less
                  than \code{sig_cutoff}.
                  (See \code{\link{assoc_scores}} for more information on
                 \code{log_OR}, \code{p_G}, and \code{sig_cutoff}.)
                  The information in the columns
                  \code{lor_min}, \code{lor_max}, and \code{lor_sd} is
                  meant as a tool in support of the interpretation of
                  the \code{S_lor} column.}
  \item{lor_max }{The highest attested \code{log_OR} measure across all
                  \code{(a,b)} couples for which \code{p_G} is less
                  than \code{sig_cutoff}.
                  (See \code{\link{assoc_scores}} for more information on
                 \code{log_OR}, \code{p_G}, and \code{sig_cutoff}.)
                  The information in the columns
                  \code{lor_min}, \code{lor_max}, and \code{lor_sd} is
                  meant as a tool in support of the interpretation of
                  the \code{S_lor} column.}
  \item{lor_sd }{The standard deviation of all \code{log_OR} values across the
                  \code{(a,b)} couples for which \code{p_G} is less
                  than \code{sig_cutoff}.
                  (See \code{\link{assoc_scores}} for more information on
                 \code{log_OR}, \code{p_G}, and \code{sig_cutoff}.)
                  The information in the columns
                  \code{lor_min}, \code{lor_max}, and \code{lor_sd} is
                  meant as a tool in support of the interpretation of
                  the \code{S_lor} column.}
  \item{S_lor }{The 'log of odds ratio stability' is a stability measure
                based on the effect size measure
                \code{log_OR}.
                (See \code{\link{assoc_scores}} for more information on
                 \code{log_OR}.)
                The measure \code{S_lor} is calculated as the mean \code{log_OR}
                across all \code{(a,b)} pairs, with this special caveat that,
                for the purpose of these calculations,
                \code{log_OR} is set to zero for all \code{(a,b)} pairs
                in which \code{p_G} is higher
                than \code{sig_cutoff}.
                (See \code{\link{assoc_scores}} for more information on
                 \code{log_OR}, \code{p_G}, and \code{sig_cutoff}.)
                Put differently,
                \code{S_lor} is calculated as a fraction with as its
                numerator the sum of \code{log_OR} values across all
                  \code{(a,b)} couples for which \code{p_G} is less 
                  than \code{sig_cutoff}, and as its
                  denominator \eqn{n * m}.
                }

}


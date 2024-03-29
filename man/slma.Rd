% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slma.R
\name{slma}
\alias{slma}
\title{Stable lexical marker analysis}
\usage{
slma(
  x,
  y,
  file_encoding = "UTF-8",
  sig_cutoff = qchisq(0.95, df = 1),
  small_pos = 1e-05,
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
  ...
)
}
\arguments{
\item{x, y}{Character vector or \code{\link{fnames}} object with filenames for the two
sets of documents.}

\item{file_encoding}{Encoding of all the files to read.}

\item{sig_cutoff}{Numeric value indicating the cutoff value for 'significance
in the stable lexical marker analysis. The default value is \code{qchist(.95, df = 1)},
which is about 3.84.}

\item{small_pos}{Alternative (but sometimes inferior) approach to dealing with
zero frequencies, compared to \code{haldane}. The argument \code{small_pos}
only applies when \code{haldane} is set to \code{FALSE}.
(See the Details section.)

If \code{haldane} is \code{FALSE}, and there is at least one zero frequency
in a contingency table, adding small positive values to the zero frequency
cells is done systematically for all measures calculated for that table,
not just for measures that need this to be done.}

\item{keep_intermediate}{Logical. If \code{TRUE}, results from intermediate
calculations are kept in the output as the "intermediate" element. This is
necessary if you want to inspect the object with the \code{\link[=details]{details()}} method.}

\item{verbose}{Logical. Whether progress should be printed to the console
during analysis.}

\item{min_rank, max_rank}{Minimum and maximum frequency rank in the first
corpus (\code{x}) of the items to take into consideration as candidate stable
markers. Only tokens or token n-grams with a frequency rank greater than or
equal to \code{min_rank} and lower than or equal to \code{max_rank} will be included.}

\item{keeplist}{List of types that must certainly be included in the list of
candidate markers regardless of their frequency rank and of \code{stoplist}.}

\item{stoplist}{List of types that must not be included in the list of candidate
markers, although, if a type is included in \code{keeplist}, its inclusion in
\code{stoplist} is disregarded.}

\item{ngram_size}{Argument in support of ngrams/skipgrams (see also \code{max_skip}).

If one wants to identify individual tokens, the value of \code{ngram_size}
should be \code{NULL} or \code{1}. If one wants to retrieve
token ngrams/skipgrams, \code{ngram_size} should be an integer indicating
the size of the ngrams/skipgrams. E.g. \code{2} for bigrams, or \code{3} for
trigrams, etc.}

\item{max_skip}{Argument in support of skipgrams. This argument is ignored if
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
\code{3}, then 3-skip 5-grams are being retrieved.}

\item{ngram_sep}{Character vector of length 1 containing the string that is used to
separate/link tokens in the representation of ngrams/skipgrams
in the output of this function.}

\item{ngram_n_open}{If \code{ngram_size} is \code{2} or higher, and moreover
\code{ngram_n_open} is a number higher than \code{0}, then
ngrams with 'open slots' in them are retrieved. These
ngrams with 'open slots' are generalizations of fully lexically specific
ngrams (with the generalization being that one or more of the items
in the ngram are replaced by a notation that stands for 'any arbitrary token').

For instance, if \code{ngram_size} is \code{4} and \code{ngram_n_open} is
\code{1}, and if moreover the input contains a
4-gram \code{"it_is_widely_accepted"}, then the output will contain
all modifications of \code{"it_is_widely_accepted"} in which one (since
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
\code{"it_is_[]_[]_that"}.}

\item{ngram_open}{Character string used to represent open slots in ngrams in the
output of this function.}

\item{...}{Additional arguments.}
}
\value{
An object of class \code{slma}, which is a named list with at least the following
elements:
\itemize{
\item A \code{scores} dataframe with information about the stability of the chosen
lexical items. (See below.)
\item An \code{intermediate} list with a register of intermediate values if
\code{keep_intermediate} was \code{TRUE}.
\item Named items registering the values of the arguments with the same name,
namely \code{sig_cutoff}, \code{small_pos}, \code{x}, and \code{y}.
}

The \code{slma} object has \code{\link[=as_data_frame]{as_data_frame()}} and \code{\link[=print.slma]{print}} methods
as well as an ad-hoc \code{\link[=details]{details()}} method. Note that the \code{\link[=print.slma]{print}}
method simply prints the main dataframe.
\subsection{Contents of the \code{scores} element}{

The \code{scores} element is a dataframe of which the rows are linguistic items
for which a stable lexical marker analysis was conducted and the columns are
different 'stability measures' and related statistics. By default, the
linguistic items are sorted by decreasing 'stability' according to the \code{S_lor}
measure.\tabular{llll}{
   Column \tab Name \tab Computation \tab Range of values \cr
   \code{S_abs} \tab Absolute stability \tab \code{S_att} - \code{S_rep} \tab \eqn{-(n*m)} -- \eqn{(n*m)} \cr
   \code{S_nrm} \tab Normalized stability \tab \code{S_abs} / \eqn{n*m} \tab -1 -- 1 \cr
   \code{S_att} \tab Stability of attraction \tab Number of \eqn{(a,b)} couples in which the linguistic item is a keyword for the \emph{A}-documents \tab 0 -- \eqn{n*m} \cr
   \code{S_rep} \tab Stability of repulsion \tab Number of \eqn{(a,b)} couples in which the linguistic item is a keyword for the \emph{B}-documents \tab 0 -- \eqn{n*m} \cr
   \code{S_lor} \tab Log of odds ratio stability \tab Mean of \code{log_OR} across all \eqn{(a,b)} couples but setting to 0 the value when \code{p_G} is larger than \code{sig_cutoff} \tab  \cr
}


\code{S_lor} is then computed as a fraction with as its numerator the sum of all
\code{log_OR} values across all \eqn{(a,b)} couples for which \code{p_G} is lower than
\code{sig_cutoff} and as its denominator \eqn{n*m}.
For more on \code{log_OR}, see the Value section on on \code{\link[=assoc_scores]{assoc_scores()}}. The final
three columns on the output are meant as a tool in support of the interpretation
of the \code{log_OR} column. Considering all \eqn{(a,b)} couples for which
\code{p_G} is smaller than \code{sig_cutoff}, \code{lor_min}, \code{lor_max} and \code{lor_sd}
are their minimum, maximum and standard deviation for each element.
}
}
\description{
This function conducts a stable lexical marker analysis.
}
\details{
A stable lexical marker analysis of the \emph{A}-documents in \code{x} versus the \emph{B}-documents
in \code{y} starts from a separate keyword analysis for all possible document couples
\eqn{(a,b)}, with \emph{a} an \emph{A}-document and \emph{b} a \emph{B}-document. If there are \emph{n}
\emph{A}-documents and \emph{m} \emph{B}-documents, then \eqn{n*m} keyword analyses are
conducted. The 'stability' of a linguistic item \emph{x}, as a marker for the
collection of \emph{A}-documents (when compared to the \emph{B}-documents) corresponds
to the frequency and consistency with which \emph{x} is found to be a keyword for
the \emph{A}-documents across all aforementioned keyword analyses.

In any specific keyword analysis, \emph{x} is considered a keyword for an \emph{A}-document
if \code{G_signed} is positive and moreover \code{p_G} is less than \code{sig_cutoff}
(see \code{\link[=assoc_scores]{assoc_scores()}} for more information on the measures). Item \emph{x} is
considered a keyword for the \emph{B}-document if \code{G_signed} is negative and moreover
\code{p_G} is less than \code{sig_cutoff}.
}
\examples{
a_corp <- get_fnames(system.file("extdata", "cleveland", package = "mclm"))
b_corp <- get_fnames(system.file("extdata", "roosevelt", package = "mclm"))
slma_ex <- slma(a_corp, b_corp)
}

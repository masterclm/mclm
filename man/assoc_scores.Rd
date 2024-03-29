% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assoc.R
\name{assoc_scores}
\alias{assoc_scores}
\alias{assoc_abcd}
\title{Association scores used in collocation analysis and keyword analysis}
\usage{
assoc_scores(
  x,
  y = NULL,
  min_freq = 3,
  measures = NULL,
  with_variants = FALSE,
  show_dots = FALSE,
  p_fisher_2 = FALSE,
  haldane = TRUE,
  small_pos = 1e-05
)

assoc_abcd(
  a,
  b,
  c,
  d,
  types = NULL,
  measures = NULL,
  with_variants = FALSE,
  show_dots = FALSE,
  p_fisher_2 = FALSE,
  haldane = TRUE,
  small_pos = 1e-05
)
}
\arguments{
\item{x}{Either an object of class \code{\link{freqlist}}
or an object of class \code{\link[=create_cooc]{cooc_info}}.

If \code{x} is a \code{\link{freqlist}}, it is interpreted as the target frequency
list (i.e. the list with the frequency of items in the target context) and
\code{y} must be a \code{\link{freqlist}} with the frequency of items in the
reference context.

If \code{x} is an object of class \code{\link[=create_cooc]{cooc_info}} instead, it is interpreted
as containing target frequency information, reference frequency information
and corpus size information.}

\item{y}{An object of class \code{\link{freqlist}} with the frequencies of the
reference context if \code{x} is also a \code{\link{freqlist}}. If \code{x} is an
object of class \code{\link[=create_cooc]{cooc_info}}, this argument is ignored.}

\item{min_freq}{Minimum value for \code{a[[i]]} (or for the frequency of an
item in the target frequency list) needed for its corresponding item to be
included in the output.}

\item{measures}{Character vector containing the association measures (or related
quantities) for which scores are requested. Supported measure names (and
related quantities) are described in \code{Value} below.

If \code{measures} is \code{NULL}, it is interpreted as short for the default selection,
i.e. \code{c("exp_a", "DP_rows", "RR_rows", "OR", "MS", "Dice", "PMI", "chi2_signed", "G_signed", "t", "fisher")}.

If \code{measures} is \code{"ALL"}, all supported measures are calculated (but not
necessarily all the variants; see \code{with_variants}).}

\item{with_variants}{Logical. Whether, for the requested \code{measures}, all
variants should be included in the output (\code{TRUE}) or only the main
version (\code{FALSE}). See also \code{p_fisher_2}.}

\item{show_dots}{Logical. Whether a dot should be shown in console each time
calculations for a measure are finished.}

\item{p_fisher_2}{Logical. only relevant if \code{"fisher"} is included in
\code{measures}. If \code{TRUE}, the p-value for a two-sided test (testing
for either attraction or repulsion) is also calculated. By default, only
the (computationally less demanding) p-value for a one-sided test is
calculated. See \code{Value} for more details.}

\item{haldane}{Logical. Should the Haldane-Anscombe correction be used?
(See the Details section.)

If \code{haldane} is \code{TRUE}, and there is at least one zero frequency
in a contingency table, the correction is used for all measures calculated
for that table, not just for measures that need this to be done.}

\item{small_pos}{Alternative (but sometimes inferior) approach to dealing with
zero frequencies, compared to \code{haldane}. The argument \code{small_pos}
only applies when \code{haldane} is set to \code{FALSE}.
(See the Details section.)

If \code{haldane} is \code{FALSE}, and there is at least one zero frequency
in a contingency table, adding small positive values to the zero frequency
cells is done systematically for all measures calculated for that table,
not just for measures that need this to be done.}

\item{a}{Numeric vector expressing how many times some tested item
occurs in the target context.
More specifically, \code{a[[i]]}, with \code{i} an integer, expresses
how many times the \code{i}-th tested item occurs in the target context.}

\item{b}{Numeric vector expressing how many times other items than the tested
item occur in the target context.
More specifically, \code{b[[i]]}, with \code{i} an integer, expresses
how many times \emph{other} items than the \code{i}-th tested item
occur in the target context.}

\item{c}{Numeric vector expressing how many times some tested
item occurs in the reference context.
More specifically, \code{c[[i]]}, with \code{i} an integer, expresses
how many times the \code{i}-th tested item occurs in the reference context.}

\item{d}{Numeric vector expressing how many times items other than the tested
item occur in the reference context.
More specifically, \code{d[[i]]}, with \code{i} an integer, expresses
how many times \emph{other} items than the \code{i}-th tested item occur
in the reference context.}

\item{types}{A character vector containing the names of the linguistic items
of which the association scores are to be calculated, or \code{NULL}. If
\code{NULL}, \code{\link[=assoc_abcd]{assoc_abcd()}} creates dummy types such as \code{"t001"},
\code{"t002"}, etc.}
}
\value{
An object of class \code{assoc_scores}. This is a kind of data frame with
as its rows all items from either the target frequency list or the reference
frequency list with a frequency larger than \code{min_freq} in the target list,
and as its columns a range of measures that express the extent to which
the items are attracted to the target context (when compared to the reference
context).
Some columns don't contain actual measures but rather additional information
that is useful for interpreting other measures.
\subsection{Possible columns}{

The following sections describe the (possible) columns in the output. All
of these measures are reported if \code{measures} is set to \code{"ALL"}. Alternatively,
each measure can be requested by specifying its name in a character vector
given to the \code{measures} argument. Exceptions are described in the sections
below.
\subsection{Observed and expected frequencies}{
\itemize{
\item \code{a}, \code{b}, \code{c}, \code{d}: The frequencies in cells \emph{a}, \emph{b}, \emph{c} and \emph{d},
respectively. If one of them is \code{0}, they will be augmented by 0.5 or \code{small_pos}
(see \code{Details}). These output columns are always present.
\item \code{dir}: The direction of the association: \code{1} in case of relative attraction
between the tested item and the target context (if \eqn{\frac{a}{m} \ge \frac{c}{n}}) and
\code{-1} in case of relative repulsion between the target item and the target
context (if \eqn{\frac{a}{m} < {c}{n}}).
\item \code{exp_a}, \code{exp_b}, \code{exp_c}, \code{exp_d}: The expected values for cells \emph{a}, \emph{b},
\emph{c} and \emph{d}, respectively. All these columns will be included if \code{"expected"}
is in \code{measures}. \code{exp_a} is also one of the default measures and is therefore included
if \code{measures} is \code{NULL}. The values of these columns are computed as follows:
\itemize{
\item \code{exp_a} = \eqn{\frac{m \times k}{N}}
\item \code{exp_b} = \eqn{\frac{m \times l}{N}}
\item \code{exp_c} = \eqn{\frac{n \times k}{N}}
\item \code{exp_d} = \eqn{\frac{n \times l}{N}}
}
}
}

\subsection{Effect size measures}{

Some of these measures are based on proportions and can therefore be
computed either on the rows or on the columns of the contingency table. Each
measure can be requested on its own, but pairs of measures can also be
requested with the first part of their name, as indicated in their corresponding
descriptions.
\itemize{
\item \code{DP_rows} and \code{DP_cols}: The difference of proportions, sometimes also
called Delta-p (\eqn{\Delta p}), between rows and columns respectively.
Both columns are present if \code{"DP"} is included in \code{measures}. \code{DP_rows}
is also included if \code{measures} is \code{NULL}.
They are calculated as follows:
\itemize{
\item \code{DP_rows} = \eqn{\frac{a}{m} - \frac{c}{n}}
\item \code{DP_cols} = \eqn{\frac{a}{k} - \frac{b}{l}}
}
\item \code{perc_DIFF_rows} and \code{perc_DIFF_cols}: These measures can be seen as
normalized versions of Delta-p, i.e. essentially the same measures divided
by the denominator and multiplied by \code{100}. They therefore express how large
the difference of proportions is, relative to the reference proportion.
The multiplication by \code{100} turns the resulting 'relative difference of
proportion' into a percentage.
Both columns are present if \code{"perc_DIFF"} is included in \code{measures}.
They are calculated as follows:
\itemize{
\item \code{perc_DIFF_rows} = \eqn{100 * \frac{(a / m) - (c / n)}{c / n}}
\item \code{perc_DIFF_cols} = \eqn{100 * \frac{(a / k) - (b / l)}{c / n}}
}
\item \code{DC_rows} and \code{DC_cols}: The difference coefficient can be seen as a
normalized version of Delta-p, i.e. essentially dividing the difference of
proportions by the sum of proportions.
Both columns are present if \code{"DC"} is included in \code{measures}.
They are calculated as follows:
\itemize{
\item \code{DC_rows} = \eqn{\frac{(a / m) - (c / n)}{(a / m) + (c / n)}}
\item \code{DC_cols} = \eqn{\frac{(a / k) - (b / l)}{(a / k) + (b / l)}}
}
\item \code{RR_rows} and \code{RR_cols}: Relative risk for the rows and columns
respectively. \code{RR_rows} represents then how large the proportion in the
target context is, relative to the proportion in the reference context.
Both columns are present if \code{"RR"} is included in \code{measures}.
\code{RR_rows} is also included if \code{measures} is \code{NULL}.
They are calculated as follows:
\itemize{
\item \code{RR_rows} = \eqn{\frac{a / m}{c / n}}
\item \code{RR_cols} = \eqn{\frac{a / k}{b / l}}
}
\item \code{LR_rows} and \code{LR_cols}: The so-called 'log ratio' of the rows and
columns, respectively. It can be seen as a transformed version of the relative
risk, viz. its binary log.
Both columns are present if \code{"LR"} is included in \code{measures}.
They are calculated as follows:
\itemize{
\item \code{LR_rows} = \eqn{\log_2\left(\frac{a / m}{c / n}\right)}
\item \code{LR_cols} = \eqn{\log_2\left(\frac{a / k}{b / l}\right)}
}
}

Other measures use the contingency table in a different way and therefore
don't have a complementary row/column pair. In order to retrieve these columns,
if \code{measures} is not \code{"ALL"}, their name must be in the \code{measures} vector.
Some of them are included by default, i.e. if \code{measures} is \code{NULL}.
\itemize{
\item \code{OR}: The odds ratio, which can be calculated either as
\eqn{\frac{a/b}{c/d}} or as \eqn{\frac{a/c}{b/d}}.
This column is present \code{measures} is \code{NULL}.
\item \code{log_OR}: The log odds ratio, which can be calculated either as
\eqn{\log\left(\frac{a/b}{c/d}\right)} or as \eqn{\log\left(\frac{a/c}{b/d}\right)}.
In other words, it is the natural log of the odds ratio.
\item \code{MS}: The minimum sensitivity, which is calculated as
\eqn{\min(\frac{a}{m}, \frac{a}{k})}.
In other words, it is either \eqn{\frac{a}{m}} or \eqn{\frac{a}{k}}, whichever is lowest.
This column is present \code{measures} is \code{NULL}.
\item \code{Jaccard}: The Jaccard index, which is calculated as
\eqn{\frac{a}{a + b + c}}. It expresses \emph{a}, which is the frequency of the
test item in the target context, relative to \emph{b + c + d}, i.e. the frequency
of all other contexts.
\item \code{Dice}: The Dice coefficient, which is calculated as
\eqn{\frac{2a}{m + k}}. It expresses the harmonic mean of \eqn{\frac{a}{m}} and \eqn{\frac{a}{k}}
This column is present \code{measures} is \code{NULL}.
\item \code{logDice}: An adapted version of the Dice coefficient. It is calculated as
\eqn{14 + \log_2\left(\frac{2a}{m + k}\right)}. In other words, it is \code{14}
plus the binary log of the Dice coefficient.
\item \code{phi}: The phi coefficient (\eqn{\phi}), which is calculated as
\eqn{\frac{(a \times d) - (b \times c)}{ \sqrt{m \times n \times k \times l}}}.
\item \code{Q}: Yule's Q, which is calculated as
\eqn{\frac{(a \times d) - (b \times c)}{(a \times d)(b \times c)}}.
\item \code{mu}: The measure mu (\eqn{\mu}), which is calculated as
\eqn{\frac{a}{\mathrm{exp\_a}}} (see \code{exp_a}).
\item \code{PMI} and \code{pos_PMI}: (Positive) pointwise mutual information,
which can be seen as a modification of the mu measure and is calculated as
\eqn{\log_2\left(\frac{a}{\mathrm{exp\_a}}\right)}. In \code{pos_PMI}, negative
values are set to \code{0}.
The \code{PMI} column is present \code{measures} is \code{NULL}.
\item \code{PMI2} and \code{PMI3}: Modified versions of \code{PMI} that aim to give relatively
more weight to cases with relatively higher \emph{a}. However, because of this
modification, they are not pure effect size measures any more.
\itemize{
\item \code{PMI2} = \eqn{\log_2\left(\frac{a^2}{\mathrm{exp\_a}}\right)}
\item \code{PMI3} = \eqn{\log_2\left(\frac{a^3}{\mathrm{exp\_a}}\right)}
}
}
}

\subsection{Strength of evidence measures}{

The first measures in this section tend to come in triples: a test statistic,
its p-value (preceded by \code{p_}) and its signed version (followed by \verb{_signed}).
The test statistics indicate evidence of either attraction or repulsion.
Thus, in order to indicate the direction of the relationship, a negative
sign is added in the "signed" version when \eqn{\frac{a}{k} < \frac{c}{l}}.

In each of these cases, the name of the main measure (e.g. \code{"chi2"})
and/or its signed counterpart (e.g. \code{"chi2_signed"}) must be in the \code{measures}
argument, or \code{measures} must be \code{"ALL"}, for the columns to be included in
the output. If the main function is requested, the signed counterpart will
also be included, but if only the signed counterpart is requested, the non-signed
version will be excluded.
For the p-value to be retrieved, either the main measure or its signed version
must be requested and, \emph{additionally}, the \code{with_variants} argument must be
set to \code{TRUE}.
\itemize{
\item \code{chi2}, \code{p_chi2} and \code{chi2_signed}: The chi-squared test statistic
(\eqn{\chi^2}) as used in a chi-squared test of independence or in a
chi-squared test of homogeneity for a two-by-two contingency table.
Scores of this measure are high when there is strong evidence for attraction,
but also when there is strong evidence for repulsion.
The \code{chi2_signed} column is present if \code{measures} is \code{NULL}.
\code{chi2} is calculated as follows: \deqn{
                        \frac{(a-\mathrm{exp\_a})^2}{\mathrm{exp\_a}} +
                        \frac{(b-\mathrm{exp\_b})^2}{\mathrm{exp\_b}} +
                        \frac{(c-\mathrm{exp\_c})^2}{\mathrm{exp\_c}} +
                        \frac{(d-\mathrm{exp\_d})^2}{\mathrm{exp\_d}}
                       }.
\item \code{chi2_Y}, \code{p_chi2_Y} and \code{chi2_Y_signed}: The chi-squared test statistic
(\eqn{\chi^2}) as used in a chi-squared test with Yates correction
for a two-by-two contingency table.
\code{chi2_Y} is calculated as follows: \deqn{
                        \frac{(|a-\mathrm{exp\_a}| - 0.5)^2}{\mathrm{exp\_a}} +
                        \frac{(|b-\mathrm{exp\_b}| - 0.5)^2}{\mathrm{exp\_b}} +
                        \frac{(|c-\mathrm{exp\_c}| - 0.5)^2}{\mathrm{exp\_c}} +
                        \frac{(|d-\mathrm{exp\_d}| - 0.5)^2}{\mathrm{exp\_d}}
                       }.
\item \code{chi2_2T}, \code{p_chi2_2T} and \code{chi2_2T_signed}: The chi-squared test statistic
(\eqn{\chi^2}) as used in a chi-squared goodness-of-fit test applied to the
first column of the contingency table. The \code{"2T"} in the name stands for
'two terms' (as opposed to \code{chi2}, which is sometimes the 'four terms' version).
\code{chi2_2T} is calculated as follows: \deqn{
                        \frac{(a-\mathrm{exp\_a})^2}{\mathrm{exp\_a}} +
                        \frac{(c-\mathrm{exp\_c})^2}{\mathrm{exp\_c}}
                       }.
\item \code{chi2_2T_Y}, \code{p_chi2_2T_Y} and \code{chi2_2T_Y_signed}: The chi-squared test statistic
(\eqn{\chi^2}) as used in a chi-squared goodness-of-fit test with Yates correction, applied to the
first column of the contingency table.
\code{chi2_2T_Y} is calculated as follows: \deqn{
                          \frac{(|a-\mathrm{exp\_a}| - 0.5)^2}{\mathrm{exp\_a}} +
                          \frac{(|c-\mathrm{exp\_c}| - 0.5)^2}{\mathrm{exp\_c}}
                         }.
\item \code{G}, \code{p_G} and \code{G_signed}: G test statistic, which is also sometimes
called log-likelihood ratio (LLR) and, somewhat confusingly, G-squared.
This is the test statistic as used in a log-likelihood ratio test for independence
or homogeneity in a two-by-two contingency table.
Scores are high in case of strong evidence for attraction, but also in case
of strong evidence of repulsion.
The \code{G_signed} column is present if \code{measures} is \code{NULL}.
\code{G} is calculated as follows: \deqn{
                  2 \left(
                  a \times \log(\frac{a}{\mathrm{exp\_a}}) +
                  b \times \log(\frac{b}{\mathrm{exp\_b}}) +
                  c \times \log(\frac{c}{\mathrm{exp\_c}}) +
                  d \times \log(\frac{d}{\mathrm{exp\_d}})
                  \right)
                 }
\item \code{G_2T}, \code{p_G_2T} and \code{G_2T_signed}: The test statistic
used in a log-likelihood ratio test for goodness-of-fit applied to the first
column of the contingency table.
The \code{"2T"} stands for 'two terms'.
\code{G_2T} is calculated as follows: \deqn{
                  2 \left(
                  a \times \log(\frac{a}{\mathrm{exp\_a}}) +
                  c \times \log(\frac{c}{\mathrm{exp\_c}})
                  \right)
                 }
}

The final two groups of measures take a different shape. The
\verb{_as_chisq1} columns compute \code{qchisq(1 - p, 1)}, with \code{p} being the p-values
they are transforming, i.e. the \code{p} right quantile in a \eqn{\chi^2}
distribution with one degree of freedom (see \code{\link[=p_to_chisq1]{p_to_chisq1()}}).
\itemize{
\item \code{t}, \code{p_t_1}, \code{t_1_as_chisq1}, \code{p_t_2} and \code{t_2_as_chisq1}:
The t-test statistic, used for a t-test for the proportion \eqn{\frac{a}{N}}
in which the null hypothesis is based on \eqn{\frac{k}{N}\times\frac{m}{N}}.
Column \code{t} is present if \code{"t"} is included in \code{measures} or if \code{measures} is
\code{"ALL"} or \code{NULL}. The other four columns are present if \code{t} is requested and if,
additionally, \code{with_variants} is \code{TRUE}.
\itemize{
\item \code{t} = \eqn{
                   \frac{
                   a/N + k/N + m/N
                   }{
                   \sqrt{((a/N)\times (1-a/N))/N}
                   }
                    }
\item \code{p_t_1} is the p-value that corresponds to \code{t} when assuming a one-tailed
test that only looks at attraction; \code{t_1_as_chisq1} is its transformation.
\item \code{p_t_2} is the p-value that corresponds to \code{t} when assuming a two-tailed
test, viz. that looks at both attraction and repulsion; \code{t_2_as_chisq1} is
its transformation.
}
\item \code{p_fisher_1}, \code{fisher_1_as_chisq1}, \code{p_fisher_1r}, \code{fisher_1r_as_chisq1}:
The p-value of a one-sided Fisher exact test.
The column \code{p_fisher_1} is present if either \code{"fisher"} or \code{"p_fisher"} are in \code{measures}
or if \code{measures} is \code{"ALL"} or \code{NULL}. The other columns are present if \code{p_fisher_1} as
been requested and if, additionally, \code{with_variants} is \code{TRUE}.
\itemize{
\item \code{p_fisher_1} and \code{p_fisher_1r} are the p-values of the Fisher exact test
that look at attraction and repulsion respectively.
\item \code{fisher_1_as_chisq1} and \code{fisher_1r_as_chisq1} are their respective transformations..
}
\item \code{p_fisher_2} and \code{fisher_2_as_chisq1}: p-value for a two-sided Fisher
exact test, viz. looking at both attraction and repulsion. \code{p_fisher_2}
returns the p-value and \code{fisher_2_as_chisq1} is its transformation.
The \code{p_fisher_2} column is present if either \code{"fisher"} or \code{"p_fisher_1"} are
in \code{measures} or if \code{measures} is \code{"ALL"} or \code{NULL} and if, additionally, \code{p_fisher_2} is
\code{TRUE}. \code{fisher_2_as_chisq1} is present if \code{p_fisher_2} was requested and,
additionally, \code{with_variants} is \code{TRUE}.
}
}

}

\subsection{Properties of the class}{

An object of class \code{assoc_scores} has:
\itemize{
\item associated \code{\link[=as.data.frame]{as.data.frame()}}, \code{\link[=print.assoc_scores]{print()}},
\code{\link[=sort.assoc_scores]{sort()}} and \code{\link[tibble:as_tibble]{tibble::as_tibble()}} methods,
\item an interactive \code{\link[=explore]{explore()}} method and useful getters, viz. \code{\link[=n_types]{n_types()}} and
\code{\link[=type_names]{type_names()}}.
}

An object of this class can be saved to file with \code{\link[=write_assoc]{write_assoc()}} and read
with \code{\link[=read_assoc]{read_assoc()}}.
}
}
\description{
\code{assoc_scores} and \code{assoc_abcd} take as their arguments co-occurrence
frequencies of a number of items and return a range of association scores used
in collocation analysis, collostruction analysis and keyword analysis.
}
\details{
\subsection{Input and output}{

\code{\link[=assoc_scores]{assoc_scores()}} takes as its arguments a target frequency list and a reference
frequency lists (either as two \code{\link{freqlist}} objects or as a
\code{\link[=create_cooc]{cooc_info}} object) and returns a number of popular measures
expressing, for (almost) every item in either one of these lists, the extent
to which the item is attracted to the target context, when compared to the
reference context. The "almost" is added between parentheses because, with
the default settings, some items are automatically excluded from the output
(see \code{min_freq}).

\code{\link[=assoc_abcd]{assoc_abcd()}} takes as its arguments four vectors \code{a}, \code{b}, \code{c}, and \code{d}, of
equal length. Each tuple of values \verb{(a[i], b[i], c[i], d[i])}, with \code{i} some
integer number between 1 and the length of the vectors, is assumed to represent
the four numbers \emph{a}, \emph{b}, \emph{c}, \emph{d} in a contingency table of the type:\tabular{rrrr}{
    \tab \strong{tested item} \tab \strong{any other item} \tab \strong{total} \cr
   target context \tab \emph{a} \tab \emph{b} \tab \emph{m} \cr
   reference context \tab \emph{c} \tab \emph{d} \tab \emph{n} \cr
   total \tab \emph{k} \tab \emph{l} \tab \emph{N} \cr
}


In the above table \emph{m}, \emph{n}, \emph{k}, \emph{l} and \emph{N} are marginal frequencies.
More specifically, \emph{m = a + b}, \emph{n = c + d}, \emph{k = a + c}, \emph{l = b + d} and \emph{N = m + n}.
}

\subsection{Dealing with zeros}{

Several of the association measures break down when one or more of the values
\code{a}, \code{b}, \code{c}, and \code{d} are zero (for instance, because this would lead to
division by zero or taking the log of zero). This can be dealt with in different
ways, such as the Haldane-Anscombe correction.

Strictly speaking, Haldane-Anscombe correction specifically applies to the
context of (log) odds ratios for two-by-two tables and boils down to adding
\code{0.5} to each of the four values \code{a}, \code{b}, \code{c}, and \code{d}
in every two-by-two contingency table for which the original values
\code{a}, \code{b}, \code{c}, and \code{d} would not allow us to calculate
the (log) odds ratio, which happens when one (or more than one) of the four
cells is zero.
Using the Haldane-Anscombe correction, the (log) odds ratio is then calculated
on the bases of these 'corrected' values for \code{a}, \code{b}, \code{c}, and \code{d}.

However, because other measures that do not compute (log) odds ratios might
also break down when some value is zero, all measures will be computed on the
'corrected' contingency matrix.

If the \code{haldane} argument is set to \code{FALSE}, division by zero or taking the
log of zero is avoided by systematically adding a small positive value to all
zero values for \code{a}, \code{b}, \code{c}, and \code{d}. The argument \code{small_pos}
determines which small positive value is added in such cases. Its default value is \code{0.00001}.
}
}
\examples{
assoc_abcd(10 , 200, 100,  300, types = "four")
assoc_abcd(30, 1000,  14, 5000, types = "fictitious")
assoc_abcd(15, 5000,  16, 1000, types = "toy")
assoc_abcd( 1,  300,   4, 6000, types = "examples")

a <- c(10,    30,    15,    1)
b <- c(200, 1000,  5000,  300)
c <- c(100,   14,    16,    4)
d <- c(300, 5000, 10000, 6000)
types <- c("four", "fictitious", "toy", "examples")
(scores <- assoc_abcd(a, b, c, d, types = types))

as_data_frame(scores)
as_tibble(scores)

print(scores, sort_order = "PMI")
print(scores, sort_order = "alpha")
print(scores, sort_order = "none")
print(scores, sort_order = "nonsense")

print(scores, sort_order = "PMI",
      keep_cols = c("a", "exp_a", "PMI", "G_signed"))
print(scores, sort_order = "PMI",
      keep_cols = c("a", "b", "c", "d", "exp_a", "G_signed"))
print(scores, sort_order = "PMI",
     drop_cols = c("a", "b", "c", "d", "exp_a", "G_signed",
                    "RR_rows", "chi2_signed", "t"))
}

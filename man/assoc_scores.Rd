\name{assoc_scores}
\alias{assoc_scores}
\alias{assoc_abcd}
\title{
Association scores used in collocation analysis and keyword analysis
}
\description{
The functions \code{assoc_scores} and \code{assoc_abcd} take as
 their arguments co-occurrence frequencies for a number of items, and
 return a range of association scores used in collocation analysis,
 collostruction analysis and keywords analysis.
}
\usage{
assoc_scores(x, 
             y = NULL, 
             min_freq = 3,
             measures = NULL,
             with_variants = FALSE,
             show_dots = FALSE,
             p_fisher_2 = FALSE,
             haldane = TRUE,
             small_pos = 0.00001)

assoc_abcd(a, b, c, d,
           types = NULL,
           measures = NULL,
           with_variants = FALSE,
           show_dots = FALSE,
           p_fisher_2 = FALSE,
           haldane = TRUE,
           small_pos = 0.00001) 
}
\arguments{
  \item{x}{
the argument \code{x} can either be an object of class \code{"freqlist"}
(i.e. the data type that is returned by the function \code{\link{freqlist}}) or
an object of class \code{"cooc_info"} (i.e. the data type that is returned by
the functions \code{\link{surf_cooc}} and \code{\link{text_cooc}}).

If \code{x} is of class \code{"freqlist"}, it is interpreted as the
target frequency list (i.e. the list with the frequencies of items in the
target context).

If \code{x} is of class \code{"cooc_info"}, it is interpreted to contain target
frequency information, reference frequency information, and corpus size information.
}
  \item{y}{
if \code{x} is of class \code{"freqlist"}, then \code{y} is expected to also be
of class \code{"freqlist"}, and is interpreted as the
reference frequency list (i.e. the list with the frequencies of items in the
reference context).

If \code{x} is of class \code{"cooc_info"}, then \code{"y"} is ignored.
}
  \item{a}{
a vector of numbers that express how many times some tested item (i.e. some candidate collocate or keyword) occurs in
the target context (i.e. in the context of the 'node' or in the target corpus). More specifically, \code{a[i]}, with \code{i} an integer,
expresses how many times the
\code{i}-th tested item occurs in the target context.
}
  \item{b}{
a vector of numbers that express how many times other items than the tested item occur in the target context. More specifically, \code{b[i]},
with \code{i} an integer, expresses
how many times other items than the \code{i}-th tested item occur in the
target context.
}
  \item{c}{
a vector of numbers that express how many times tested item occurs in
the reference context. More specifically, \code{c[i]}, with \code{i} an integer,
expresses how many times the
\code{i}-th tested item occurs in the reference context.
}
  \item{d}{
a vector of numbers that express how many times other items than the
tested item occur in the reference context. More specifically,
\code{d[i]}, with \code{i} an integer, expresses
how many times other items than the  \code{i}-th tested item occur in the
reference context.
}
  \item{types}{
  either \code{NULL} or a character vector containing the names of the
  linguistics items of which the association scores are to be calculated.
  If \code{types} is \code{NULL}, then the function \code{assoc_abcd} creates
  dummy types such as \code{"t001"}, \code{"t002"}, etc. In the case of the
  function \code{assoc_scores}, the types are retrieved from the
  argument \code{x}.
}
  \item{min_freq}{
the minimum value for \code{a[i]} that is needed for item \code{i} to be
included in the output.
}
  \item{measures}{
a character vector containing the association measures (or related quantities)
 for which scores are
 requested. Supported measure names (and related quantities) are
 \code{"exp_a"}, \code{"exp_b"},
 \code{"exp_c"}, \code{"exp_d"}, \code{"DP_rows"}, \code{"DP_cols"}, and many others.
 Below, in de section on the output \code{Value}, all supported measures (and
 their names) are described.
 The argument \code{measures}
 can also have the value \code{NULL}, which is interpreted as short for the default
 selection
 \code{c("exp_a", "DP_rows", "RR_rows",
         "OR", "MS", "Dice", "PMI",
         "chi2_signed", "G_signed",
         "t", "fisher")}.
 The argument \code{measures}
 can also have the value \code{"ALL"}, in which case all supported measures are
 calculated (but not necessarily all variants; see the section on the
 output \code{Value}).
 }
  \item{with_variants}{
a boolean value that expresses whether for the requested \code{measures} all
 variants should be included in the output (\code{TRUE}) or just the
 main versions (\code{FALSE}). Also, see \code{p_fisher_2}.
}
  \item{show_dots}{
a boolean value that expresses whether or not a dot (\code{.}) should be
  output to the console each time calculations for a measure are finished.
}
  \item{p_fisher_2}{
a boolean value that expresses whether, in case \code{"fisher"} is one of
 the requested measures, the p-value for a two-sided test (testing for
 either attraction or repulsion) should also be
 calculated. By default, only the (computationally less demanding)
 p-value for a one-sided test is calculating. See the section on the
 output \code{Value} for more details.
}
  \item{haldane}{
Boolean value that determines whether or not  
 Haldane-Anscombe correction is used. Stricly speaking, Haldane-Anscombe correction 
 specifically applies to the context of (log) odds ratios for two-by-two tables and boils down
 to adding \code{0.5} to each of the four values \code{a}, \code{b}, \code{c}, and \code{d}
 in every two-by-two contingency table for which the original values
 \code{a}, \code{b}, \code{c}, and \code{d} would not allow us to calculate the (log) odds
 ratio, which happes when one (or more than one) 
 of the four cells is zero. Using the Haldane-Anscombe correction, the (log) odds ratio 
 is then calculated on the
 bases of these 'corrected' values for \code{a}, \code{b}, \code{c}, and \code{d}.
 
 However, similar issues also arise in several of the other association measures supported
 by \code{assoc_scores} and \code{assoc_abcd}. Several other measures can also break down when
 one or more of the values \code{a}, \code{b}, \code{c}, and \code{d} are
 zero, because the calculations would lead to division by zero or taking
 the log of zero. Therefore, when the argument \code{haldane} is set to \code{TRUE},
 all measures supported by by \code{assoc_scores} and \code{assoc_abcd} work
 with the same corrections for \code{a}, \code{b}, \code{c}, and \code{d}.

 If \code{haldane} is \code{TRUE}, and there is at least one zero frequency in a contingency table, 
 the correction is used for all measures calculated for that table,
 not just for measures that need this to be done. 

}
  \item{small_pos}{
Alternative (but sometimes inferior) approach to dealing with zero frequencies. The
 argument \code{small_pos} only applies when \code{haldane} is set to \code{FALSE}.

Several of the association measures break down when
 one or more of the values \code{a}, \code{b}, \code{c}, and \code{d} are
 zero (for instance, because this would lead to division by zero or taking
 the log of zero). If \code{haldane} is \code{FALSE}, division by zero or
 taking the log of zero is avoided by systematically adding a small positive value 
 to all zero values for \code{a}, \code{b}, \code{c}, and \code{d}. The
 argument \code{small_pos} determines which small positive value is added
 in such cases. Its default value is \code{0.00001}.
 
 If \code{haldane} is \code{FALSE}, and there is at least one zero frequency in a contingency table, 
 adding small positive values to the zero frequency cells is done systematically for all measures
 calculated for that table,
 not just for measures that need this to be done. 
 
}
}
\details{
The function \code{assoc_scores} takes as its argument a
 target frequency list and a reference frequency list and
 returns a number of popular measures that express, for
 (almost) every item in either one of these lists, the extent
 to which
 the item is attracted to the target context (when compared
 to the reference context). The "almost", between parentheses,
 is added, because with the default settings, some items
 are automatically excluded from the output (see \code{min_freq}).

The function \code{assoc_abcd} takes as its arguments four
 vectors \code{a}, \code{b}, \code{c} and
 \code{d} of equal length. Each tuple of values
 \code{(a[i],b[i],c[i],d[i])}, with \code{i} some integer
 number between one and the length of the vectors, is
 assumed to represent the four numbers \var{a}, \var{b},
 \var{c}, \var{d} in a
 contingency table of the type

\tabular{rccl}{
                      \tab tested item  \tab any other item  \tab \cr
    target context    \tab \var{a}      \tab \var{b}    \tab \var{m}\cr
    reference context \tab \var{c}      \tab \var{d}     \tab \var{n}\cr
                      \tab \var{k}      \tab \var{l}     \tab \var{N}\cr
  }
  
 In the above table \var{m}, \var{n}, \var{k}, \var{l}, and \var{N}
 are marginal frequencies. More specifically, \var{m} = \var{a} +
 \var{b}, \var{n} = \var{c} + \var{d}, \var{k} = \var{a} + \var{c},
 \var{l} = \var{b} + \var{d}, and \var{N} = \var{m} + \var{n}.
 
}
\value{
Returns an object of the class \code{"assoc_scores"}, which is a
kind of data frame with as its rows all items from either the target
 frequency list or the reference frequency list (or, in case the argument
 \code{min_freq} is non-zero, all items for which the frequency in the
 target frequency list is a least
 \code{min_freq}), and with as its columns a range of measures that
 express the extent to which the items are attracted to the target
 context (when compared to the reference context). Some columns don't
 contain actual measures, but rather additional information that is useful
 for interpreting certain measures.

The following are (possible) columns in the output:

  \item{a   }{The frequency in cell \var{a},
              possibly augmented by \code{0.5} or by \code{small_pos} (see the arguments
               \code{haldane} and \code{small_pos}).
              This output column is always present.}
  \item{b   }{The frequency in cell \var{b},
              possibly augmented by \code{0.5} or by \code{small_pos} (see the arguments
               \code{haldane} and \code{small_pos}).
              This output column is always present.}
  \item{c   }{The frequency in cell \var{c},
               possibly augmented by \code{0.5} or by \code{small_pos} (see the arguments
               \code{haldane} and \code{small_pos}).
               This output column is always present.}
  \item{d   }{The frequency in cell \var{d},
              possibly augmented by \code{0.5} or by \code{small_pos} (see the arguments
               \code{haldane} and \code{small_pos}).
              This output column is always present.}
  \item{dir }{The direction of the association. It contains the value \code{1}
              in case of relative attraction between the tested item and the target
              context (i.e. in case \eqn{a / m \ge c / n}), and it contains
              the value \code{-1} in case of relative repulsion between the
              target item and the target context (i.e. in case
              \eqn{a / m < c / n}).
              This output column is always present. }
  \item{exp_a }{The expected value for the cell \var{a}, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(m * k)/N}.
                
                This output column is present if \code{measures}
                includes either the value \code{"exp_a"} or the value \code{"expected"}.
                It is also present if \code{measures} is \code{NULL} or
                is equal to \code{"ALL"}.}
  \item{exp_b }{The expected value for the cell \var{b}, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(m * l)/N}.
                
                This output column is present if \code{measures}
                includes either the value \code{"exp_b"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to \code{"ALL"}.}
  \item{exp_c }{The expected value for the cell \var{c}, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(n * k)/N}.
                
                This output column is present if \code{measures}
                includes either the value \code{"exp_c"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to  \code{"ALL"}.}
  \item{exp_d }{The expected value for the cell \var{d}, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(n * l)/N}.
                
                This output column is present if \code{measures}
                includes either the value \code{"exp_d"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to  \code{"ALL"}.}
  \item{DP_rows }{The difference of proportions, sometimes also
                  called Delta-p (\eqn{\Delta p}), between the rows.  This value is calculated
                  as \eqn{(a / m) - (c / n)}.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"DP_rows"}
                  or the
                  value \code{"DP"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{DP_cols }{The difference of proportions, sometimes also
                  called Delta-p (\eqn{\Delta p}), between the columns.
                  This value is calculated
                  as \eqn{(a / k) - (b / l)}.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"DP_cols"}
                  or the
                  value \code{"DP"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{perc_DIFF_rows }{The measure \%DIFF can be seen as a normalized version
                  of Delta-p (see \code{DP_rows}). The score for \%DIFF for
                  the rows is
                  calculated
                  as \deqn{100 * ((a / m) - (c / n))/(c / n)}
                  
                  This
                  essentially is Delta-p divided by \eqn{(c / n)} and then
                  multiplied by \eqn{100}. So it expresses how large the
                  difference of proportions is, relative to the reference
                  proportion. The multiplication by \eqn{100} turns the
                  resulting `relative difference of proportion' into a
                  percentage.
                  
                  This output column is present if
                  \code{measures} includes either the value
                  \code{"perc_DIFF"}
                  or the
                  value \code{"perc_DIFF_rows"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{perc_DIFF_cols }{The measure \%DIFF can be seen as a normalized version
                  of Delta-p (see \code{DP_cols}). The score for \%DIFF for
                  the columns is calculated
                  as
                  \deqn{100 * ((a / k) - (b / l))/(b / l)}
                  This
                  essentially is Delta-p divided by \eqn{(b / l)} and then
                  multiplied by \eqn{100}.
                  
                  This output column is present if
                  \code{measures} includes either the value
                  \code{"perc_DIFF"}
                  or the
                  value \code{"perc_DIFF_cols"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{DC_rows }{ The difference coefficient (DC) can be seen as another
                  normalized version
                  of Delta-p (see \code{DP_rows}). The score for DC for
                  the rows is
                  calculated
                  as
                  \deqn{((a / m) - (c / n))/((a / m) + (c / n))}
                  This
                  essentially is Delta-p divided by \eqn{((a / m) + (c / n))}.
                  So it expresses how large the
                  difference of proportions is, relative to the sum of
                  both proportions.
                  
                  This output column is present if
                  \code{measures} includes either the value
                  \code{"DC"}
                  or the
                  value \code{"DC_rows"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{DC_cols }{ The difference coefficient (DC) can be seen as another
                  normalized version
                  of Delta-p (see \code{DP_cols}). The score for DC for
                  the columns is
                  calculated
                  as
                  \deqn{((a / k) - (b / l))/((a / k) + (b / l))}
                  This
                  essentially is Delta-p divided by \eqn{((a / k) + (b / l))}.
                  
                  This output column is present if
                  \code{measures} includes either the value
                  \code{"DC"}
                  or the
                  value \code{"DC_cols"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{RR_rows }{ The relative risk for the rows.  This value is calculated
                  as \eqn{(a / m) / (c / n)}. So it expresses how large
                  the proportion in the target context is, relative to
                  the proportion in the reference context.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"RR_rows"}
                  or the
                  value \code{"DP"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{RR_cols }{ The relative risk for the columns.  This value is calculated
                  as \eqn{(a / k) / (b / l)}.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"RR_cols"}
                  or the
                  value \code{"RR"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{LR_rows }{ The so-called `log ratio' for the rows.
                   This value can be seen as
                   a transformed version of the relative risk (see
                   \code{RR_rows}). It is calculated
                  as
                  \deqn{log2((a / m) / (c / n))}
                  In other words, it is
                  the binary log of the relative risk.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"LR_rows"}
                  or the
                  value \code{"LR"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.}
  \item{LR_cols }{ The `log ratio' for the columns.  This value can be seen as
                   a transformed version of the relative risk (see
                   \code{RR_cols}). It is calculated
                  as
                  \deqn{log2((a / k) / (b / l))}
                  In other words, it is
                  the binary log of the relative risk.
                  
                  This output column is present if
                  \code{measures} includes either the value \code{"LR_cols"}
                  or the
                  value \code{"LR"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{OR }{ The odds ratio.  This value can be calculated
                  as either \eqn{(a / b) / (c / d)} or
                  \eqn{(a / c) / (b / d)}.
                  
                  This output column is present if
                  \code{measures} includes the value \code{"OR"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{log_OR }{ The log of the odds ratio.  This value is a modified
                  version of the odds ratio. It can be calculated
                  as either \eqn{log((a / b) / (c / d))} or
                  \eqn{log((a / c) / (b / d))}. In other words, it
                  is the natural log of the odds ration.
                  
                  This output column is present if
                  \code{measures} includes the value \code{"log_OR"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{MS }{ The minimum sensitivity.  This value can be calculated
                  as
                  \deqn{min(a / m,  a / k)}
                  In other words, it
                  is defined to be either \eqn{a / m} or \eqn{a / k},
                  whichever is the lowest.
                  
                  This output column is present if
                  \code{measures} includes the value \code{"MS"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.}
  \item{Jaccard }{ The Jaccard index.  This value can be calculated
                  as \eqn{a / (a + b + c)}. It expresses 
                  \eqn{a}, which is the size of the intersection
                  of the first column and the first row of the
                  table, relative to \eqn{a + b+ c}, which is the size of
                  the union
                  of the first column and the first row of the
                  tabel.
                  
                  This output column is present if
                  \code{measures} includes the value \code{"Jaccard"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{Dice }{ The Dice coefficient.  This value can be calculated
                  as \eqn{(2 * a) / (m + k)}. In expresses the
                  harmonic mean of \eqn{a / m} and \eqn{a / k}.
                  
                  This output column is present if
                  \code{measures} includes the value \code{"Dice"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{logDice }{ The logDice measure is an adapted version of
                   the Dice coefficient. It is calculated as
                    \eqn{14 + log2((2 * a) / (m + k))}. In
                    other words, it is calculated as \eqn{14} plus
                    the binary log of the Dice coefficient.
                    
                    This output column is present if
                  \code{measures} includes the value \code{"logDice"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. 
                   }
  \item{phi }{ The phi coefficient (\eqn{\phi}). It is calculated as
               \deqn{ ((a * d) - (b * c)) /
                      \sqrt (m * n * k * l)}.
                
                This output column is present if
                  \code{measures} includes the value \code{"phi"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                } 
  \item{Q }{ Yule's Q. It is calculated as
               \deqn{ ((a * d) - (b * c)) /
                      ((a * d) . (b * c))}.
                
                This output column is present if
                  \code{measures} includes the value \code{"Q"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{mu }{ The measure mu (\eqn{\mu}). It is calculated as
               \eqn{ (a / exp_a)}. See \code{"exp_a"} for what is
               meant by \eqn{exp_a}.
                
                This output column is present if
                  \code{measures} includes the value \code{"mu"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{PMI }{ The pointwise mutual information. It can be seen
               as a modification to the mu measure. It is calculated
               as is calculated as
               \eqn{ log2(a / exp_a)}. In other words, it is the
               binary log of the mu measure.
                
                This output column is present if
                  \code{measures} includes the value \code{"PMI"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{pos_PMI }{ The positive pointwise mutual information (PPMI) is the
                   same as the PMI measure, except that all negative
                   scores have been set to zero.
                
                This output column is present if
                  \code{measures} includes the value \code{"pos_PMI"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{PMI2 }{ The measure PMI2 is a modified
                version of the PMI measure. It is calculated
                as
               \eqn{ log2(a^2 / exp_a)}. The purpose of taking the
               square of \eqn{a} is to give relatively more weight to
               cases with
               relative higher \eqn{a}. Because of this modification,
               PMI2 no longer is a pure effect size measure. 
                
                This output column is present if
                  \code{measures} includes the value \code{"PMI2"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{PMI3 }{ The measure PMI3  is a modified
                version of the PMI measure. It is calculated
                as
               \eqn{ log2(a^3 / exp_a)}. The purpose of taking the
               third power of \eqn{a} is to give relatively more weight to
               cases with
               relative higher \eqn{a}. Because of this modification,
               PMI3 no longer is a pure effect size measure. 
                
                This output column is present if
                  \code{measures} includes the value \code{"PMI3"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{chi2 }{ The chi-squared test statistic (\eqn{\chi}-squared) as used in a
                chi-squared test of independence or a
                chi-squared test of homogeneity for
                a two-by-two contingency table. Scores for this measure
                are high when there is strong evidence for attraction, but
                also when there is strong evidence for repulsion.
                The measure is calculated as
                \deqn{   
                        (a - exp_a)^2 / exp_a +
                        (b - exp_b)^2 / exp_b +
                        (c - exp_c)^2 / exp_c +
                        (d - exp_d)^2 / exp_d 
                     }
                
                This output column is present if
                  \code{measures} includes the value \code{"chi2"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
              }
  \item{p_chi2 }{ The p-value that corresponds to the \code{"chi2"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"chi2"} or \code{"chi2_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{chi2_signed }{ A version of the chi-squared test statistic
                       \code{chi2} to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}.
                       
                      This output column is present if
                  \code{measures} includes the value \code{"chi2_signed"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{chi2_Y }{ The chi-squared test statistic (\eqn{\chi^2}) as used in a
                chi-squared test with Yates correction for
                a two-by-two contingency table.
                
                It is calculated as
                \deqn{  (|a - exp_a| - 0.5)^2 / exp_a +
                        (|b - exp_b| - 0.5)^2 / exp_b +
                        (|c - exp_c| - 0.5)^2 / exp_c +
                        (|d - exp_d| - 0.5)^2 / exp_d}
                
                This output column is present if
                  \code{measures} includes the value \code{"chi2_Y"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. 
                }
  \item{p_chi2_Y }{ The p-value that corresponds to the \code{"chi2_Y"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"chi2_Y"} or \code{"chi2_Y_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
                  }
  \item{chi2_Y_signed }{ A version of the \code{chi2_Y} test statistic
                        to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}. }
  \item{chi2_2T }{ The chi-squared test statistic (\eqn{\chi}-squared) as used
                   in a chi-squared goodness-of-fit test applied to the
                   first column of the contingency table.
                   The `2T' in the name stands for `two terms' (as
                   opposed to \code{"chi2"}, which is sometimes the
                   `four terms' version).
                   The measure is calculated as
                \deqn{   
                        (a - exp_a)^2 / exp_a +
                        (c - exp_c)^2 / exp_c 
                     }
                     
                     This output column is present if
                  \code{measures} includes the value \code{"chi2_2T"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                     }
  \item{p_chi2_2T }{ The p-value that corresponds to the \code{"chi2_2T"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"chi2_2T"} or \code{"chi2_2T_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{chi2_2T_signed }{ A version of 
                       \code{chi2_2T} to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}.
                       
                       This output column is present if
                  \code{measures} includes the value \code{"chi2_2T_signed"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                       }
  \item{chi2_2T_Y }{ The chi-squared test statistic (\eqn{\chi}-squared) as used
                   in a chi-squared goodness-of-fit test with Yates correction,
                   applied to the
                   first column of the contingency table. It is calculated as
                \deqn{   
                        (|a - exp_a| - 0.5)^2 / exp_a +
                        (|c - exp_c| - 0.5)^2 / exp_c 
                     }
                     
                     This output column is present if
                  \code{measures} includes the value \code{"chi2_2T_Y"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                     }
  \item{p_chi2_2T_Y }{ The p-value that corresponds to the \code{"chi2_2T_Y"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"chi2_2T_Y"} or \code{"chi2_2T_Y_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
                  }
  \item{chi2_2T_Y_signed }{ A version of 
                       \code{chi2_2T_Y} to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}.
                       
                       This output column is present if
                  \code{measures} includes the value \code{"chi2_2T_Y_signed"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                       }
  \item{G }{ G test statistic. This measure is also
             sometimes called log-likelihood ratio (LLR).
             Somewhat confusingly, is is also sometimes called
             G-squared. This is the test statistic as used in
             a log-likelihood ratio test for testing independence
             or homogeneity in a two-by-two contingency table.
             Scores are high in case of strong evidence for
             attraction, but also in case of string evidence for
             repulsion.
             The measure is calculated as
                \deqn{  2 * (a * log(a / exp_a) +
                             b * log(b / exp_b) +
                             c * log(c / exp_c) +
                             d * log(d / exp_d))
                     }
                     
                    This output column is present if
                  \code{measures} includes the value \code{"G"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}. }
  \item{p_G }{ The p-value that corresponds to the \code{"G"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"G"} or \code{"G_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{G_signed }{ A version of 
                       \code{G} to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}.
                       
                       This output column is present if
                  \code{measures} includes the value \code{"G_signed"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
                       }
  \item{G_2T }{ This is the test statistic as used in
             a log-likelihood ratio test for goodness-of-fit applied
             to the first column of the contingency table.
             The `2T' in the name stands for `two terms' (as
                   opposed to \code{"chi2"}, which is sometimes the
                   `four terms' version).
             It is calculated as
                \deqn{  2 * (a * log(a / exp_a) +
                             c * log(c / exp_c) )
                     }
                     
               This output column is present if
                  \code{measures} includes the value \code{"G_2T"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.}
  \item{p_G_2T }{ The p-value that corresponds to the \code{"G_2T"}
                  test statistic.
                  
                  This output column is present when
                  either \code{"G_2T"} or \code{"G_2T_signed"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{G_2T_signed }{ A version of 
                       \code{G_2T} to which a sign has been added.
                       More specifically, a negative sign was added
                       if \eqn{(a / k)} is smaller than
                       \eqn{(c / l)}.
                       
                       This output column is present if
                  \code{measures} includes the value \code{"G_2T_signed"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.}
  \item{t }{ The t test statistic. This is the test statistic for a
             t-test for the proportion \eqn{a / N} in which the
             null hypothesis is based on \eqn{k / N * m / N}.
             It is calculated as
             \deqn{((a / N - k / N * m / N) /
                   \sqrt(((a / N) * (1 - a / N)) / N))
                  }
                  
                       This output column is present if
                  \code{measures} includes the value \code{"t"}.
                  It is also present if \code{measures} is equal to
                  \code{"ALL"}.
             }
  \item{p_t_1 }{ The p-value that corresponds to the measure \code{"t"} when
                  assuming a one-tailed test that only looks at
                  attraction.
                  
                  This output column is present when
                  \code{"t"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
                  }
  \item{t_1_as_chisq1 }{ A transformation of \code{"p_t_1"}.
    If we refert to the score for \code{"p_t_1"} as p, then
    \code{"t_1_as_chisq1"} returns the `p right quantile' in the
    chi-square distribution with one degree of freedom.

                  This output column is present when
                  \code{"t"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.    
    }
  \item{p_t_2 }{ The p-value that corresponds to the measure
                  \code{"t"} when
                  assuming a two-tailed test that looks at
                  both attraction and repulsion.
                  
                                    This output column is present when
                  \code{"t"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{t_2_as_chisq1 }{ A transformation of \code{"p_t_2"}.
    If we refer to the score for \code{"p_t_2"} as p, then
    \code{"t_2_as_chisq1"} returns the `p right quantile' in the
    chi-square distribution with one degree of freedom.
    
                      This output column is present when
                  \code{"t"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
}
  \item{p_fisher_1 }{ p-value for a one-sided Fisher exact test,
    which only looks at attraction.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested.
    }
  \item{fisher_1_as_chisq1 }{ A transformation of
   \code{"p_fisher_1"}.
    If we refer to the score for \code{"p_fisher_1"} as p, then
    \code{"fisher_1_as_chisq"} returns the `p right quantile' in the
    chi-square distribution with one degree of freedom.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
    }
  \item{p_fisher_1r }{ p-value for a one-sided Fisher exact test,
    which only looks at repulsion.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.
    }
  \item{fisher_1r_as_chisq1 }{ A transformation of
   \code{"p_fisher_1r"}.
    If we refer to the score for \code{"p_fisher_1r"} as p, then
    \code{"fisher_1r_as_chisq1"} returns the `p right quantile' in the
    chi-square distribution with one degree of freedom.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested and moreover
                  \code{with_variants} is 
                  \code{TRUE}.}
  \item{p_fisher_2 }{ p-value for a two-sided Fisher exact test,
    which looks at both attraction and repulsion.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested and moreover 
                  \code{p_fisher_2} is 
                  \code{TRUE}.}
  \item{fisher_2_as_chisq1 }{ A transformation of
     \code{"p_fisher_2"}.
     If we refer to \code{"p_fisher_2"} as p, then
      \code{"fisher_2_as_chisq1"} returns the
      `p right quantile' in the
       chi-square distribution with one degree of freedom.
    
    This output column is present when
                  either \code{"fisher"} or \code{"p_fisher_1"} is
                  requested and moreover both
                  \code{p_fisher_2} and \code{with_variants} are 
                  \code{TRUE}.}
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

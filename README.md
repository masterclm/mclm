
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mclm <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->
<!-- badges: end -->

The goal of mclm is to gather various functions in support of
quantitative corpus linguistics. It contains classes for corpus files,
frequency lists, association scores dataframes and concordances and
functions to create them, manipulate them and read them from and write
them to files.

The package is a companion to the Methods in Corpus Linguistics course
at the Advanced Master in Linguistics (KU Leuven), but can be used for
basic corpus linguistic analyses. In particular, it offers a number of
learnr tutorials on how to perform basic tasks with mclm and filter
objects with PERL-flavor regular expressions.

## Installation

You can install the development version of mclm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("montesmariana/mclm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mclm)
#> Loading required package: ca
#> Loading required package: tibble
#> 
#> Attaching package: 'mclm'
#> The following object is masked from 'package:tibble':
#> 
#>     as_data_frame
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentences. And it lived happily ever after."
 
flist <- freqlist(toy_corpus, as_text = TRUE)
print(flist, n = 5)
#> Frequency list (types in list: 19, tokens in list: 21)
#> rank      type abs_freq nrm_freq
#> ---- --------- -------- --------
#>    1         a        2  952.381
#>    2        it        2  952.381
#>    3     after        1  476.190
#>    4       and        1  476.190
#>    5 consisted        1  476.190
#> ...

corpus_files <- get_fnames(system.file("extdata", "cleveland", package = "mclm"))
length(corpus_files)
#> [1] 4

surf <- surf_cooc(corpus_files, "government", w_left = 5, w_right = 5)
assoc_scores(surf)
#> Association scores (types in list: 77)
#>      type   a    PMI G_signed|   b    c     d dir   exp_a DP_rows
#>  1    the 230  0.578   39.554|1321 2152 20276   1 154.072   0.052
#>  2     of 136  0.403   11.259|1415 1454 20974   1 102.844   0.023
#>  3     to  57  0.286    2.323|1494  666 21762   1  46.765   0.007
#>  4     by  39  1.017   17.223|1512  259 22169   1  19.275   0.014
#>  5     in  37  0.038    0.028|1514  520 21908   1  36.028   0.001
#>  6   this  37  1.811   45.360|1514  126 22302   1  10.543   0.018
#>  7    and  36 -0.634   -8.873|1515  828 21600  -1  55.885  -0.014
#>  8      a  28  0.207    0.600|1523  347 22081   1  24.256   0.003
#>  9    has  18  1.238   11.232|1533  100 22328   1   7.632   0.007
#> 10     be  15 -0.332   -0.927|1536  277 22151  -1  18.887  -0.003
#> 11   that  15 -0.067   -0.036|1536  228 22200  -1  15.718   0.000
#> 12    for  14 -0.185   -0.258|1537  232 22196  -1  15.912  -0.001
#> 13   with  14  0.136    0.130|1537  183 22245   1  12.742   0.001
#> 14  their  13  0.112    0.082|1538  173 22255   1  12.031   0.001
#> 15  which  10 -0.120   -0.076|1541  158 22270  -1  10.867  -0.001
#> 16     as   9 -0.128   -0.078|1542  143 22285  -1   9.832  -0.001
#> 17   made   9  1.393    6.903|1542   44 22384   1   3.428   0.004
#> 18    our   9 -0.297   -0.440|1542  162 22266  -1  11.061  -0.001
#> 19 states   9  0.491    1.012|1542   90 22338   1   6.403   0.002
#> 20   been   8  0.169    0.114|1543  102 22326   1   7.115   0.001
#> ...
#> <number of extra columns to the right: 7>

conc(corpus_files, "government")
#> Concordance-based data frame (number of observations: 28)
#> idx                           left|  match   |right                         
#>   1 ...ir power and right of self-|government|they have committed to one ...
#>   2 ...he strength and safety of a|government|by the people. In each succ...
#>   3 ...the surest guaranty of good|government|. But the best results in t...
#>   4 ...sults in the operation of a|government|wherein every citizen has a...
#>   5 ...its which our happy form of|government|can bestow. On this auspici...
#>   6 ...ion of a republican form of|government|and most compatible with th...
#>   7 ... In the administration of a|government|pledged to do equal and exa...
#>   8 ...enefits of the best form of|government|ever vouchsafed to man. And...
#>   9 ...na. The admitted right of a|government|to prevent the influx of el...
#>  10 ...ure of that sovereign self-|government|pertaining to the States of...
#>  11 ...s land of freedom, of self-|government|, and of laws, here peaceab...
#>  12 ...f successful constitutional|government|, maintenance of good faith...
#>  13 ...ty pending with any foreign|government|. The Argentine Government ...
#>  14 ...ation in favor of a foreign|government|upon the right of selection...
#>  15 ...everal States into a single|government|. In these contests between...
#>  16 ...nd complications of distant|government|s. Therefore I am unable to...
#>  17 ...na. The admitted right of a|government|to prevent the influx of el...
#>  18 ...ngo has been organized as a|government|under the sovereignty of Hi...
#>  19 ... plenipotentiaries of other|government|s, thus making the United S...
#>  20 ...rpose toward their original|government|s. These evils have had man...
#>  21 ...e safety and welfare of any|government|. Emergency calling for an ...
#>  22 ... at legations. Some foreign|government|s do not recognize the unio...
#>  23 ... President shall invite the|government|s of the countries composin...
#>  24 ...ttitude and intent of those|government|s in respect of the establi...
#>  25 ...ned that the views of these|government|s are in each instance supp...
#>  26 ...ted by a republican form of|government|, to which they owe allegia...
#>  27 ... the people who desire good|government|, having secured this statu...
#>  28 ...for the use of the District|government|which shall better secure t...
#> 
#> This data frame has 6 columns:
#>    column
#> 1 glob_id
#> 2      id
#> 3  source
#> 4    left
#> 5   match
#> 6   right
```

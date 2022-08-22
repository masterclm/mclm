## usethis namespace: start
#' @useDynLib mclm, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp evalCpp
## usethis namespace: end
NULL

#' Mastering Corpus Linguistic Methods
#' 
# Read corpus files, create frequency lists, generate concordances, compute co-occurrence
# frequencies and perform keyword analysis, stable lexical markers analysis
# and more. Companion to the Methods in Corpus Linguistics course at the Advanced
# Master in Linguistics (KU Leuven).
#' 
#' 
#' @importFrom ca ca
#' @importFrom crayon bold
#' @importFrom dplyr bind_rows mutate
#' @importFrom graphics par
#' @importFrom grDevices rgb
#' @importFrom readr locale read_lines write_lines
#' @importFrom stats fisher.test pchisq phyper pt qchisq sd complete.cases
#' @importFrom stringr str_sort str_trim str_trunc
#' @importFrom stringi stri_unescape_unicode stri_pad_left stri_pad_both stri_pad_right
#' @importFrom tibble as_tibble tibble
#' @importFrom tm DirSource TermDocumentMatrix VCorpus
#' @importFrom utils type.convert
#' @importFrom XML xmlValue
#' @importFrom yaml as.yaml yaml.load
#' @keywords internal
"_PACKAGE"
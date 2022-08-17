#' Build skipgrams
#' 
#' This function builds skipgrams. It's not meant for end users, it's called
#' by the internal function [build_ngrams()].
#'
#' @param x An object of class [`tokens`]
#' @param ngram_size Requested size of the skipgrams.
#' @param max_skip Maximum number of tokens to be skipped.
#' @param sep Separator used to join items within a skipgram.
#'
#' @return A character vector
#' @noRd
skipgrams <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  skipgramsC(x, ngram_size, max_skip, sep)
}

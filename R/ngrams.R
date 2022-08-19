# TODO: Some of the functions below either (i) will have to be 'translated'...
#    from a 'for-loop' approach to an 'lapply' approach or (ii) will have to be
#    adapted so that they use mutable objects (e.g. environments).

list_paste <- function(x, sep = "_") {
  if (length(x) == 0) {
    character(0)
  } else if (length(x) == 1) {
    x[[1]]
  } else {
    paste(x[[1]], list_paste(x[-1], sep = sep), sep = sep)
  }
}

list_paste_open <- function(x,
                            first_pos = 1,
                            sep = "_",
                            open_pos = numeric(0),
                            open = "[]") {
  if (length(x) == 0) {
    character(0)
  } else {
    if (first_pos %in% open_pos) {
      car <- rep(open, length(x[[1]]))
    } else {
      car <- x[[1]]
    }
    if (length(x) == 1) {
      car
    } else {
      paste(car,
            list_paste_open(x[-1], first_pos = first_pos + 1, sep = sep,
                            open_pos = open_pos, open = open),
            sep = sep)
      
    }
  }
}

build_open_pos <- function(ngram_size, n_open) {
  result <- list()
  if ((n_open > 0) && (ngram_size - n_open > 1)) {
    result <- as.list(2:(ngram_size - n_open))
    n_open <- n_open - 1
  }
  if (length(result) > 0) {
    while (n_open > 0) {
      old_result <- result
      result <- list()
      for (item in old_result) {
        last <- item[length(item)]
        for (i in (last + 1):(ngram_size - n_open)) {
          result[[length(result) + 1]] <- c(item, i)
        }
      }
      n_open <- n_open - 1
    }
  }
  result
}

#' Build an n-gram
#'
#' @param x An object of class [`tokens`]
#' @param ngram_size Number of items in the ngram.
#' @param max_skip If larger than 1, [skipgrams()] wil be called.
#' @param sep Character vector to join the elements in the ngram/skipgram.
#' @param n_open Number of open slots.
#' @param open Character vector used to represent open slots.
#'
#' @return Character vector
#' @noRd
build_ngrams <- function(x,
                         ngram_size = 3,
                         max_skip = 0,
                         sep = "_",
                         n_open = 0,
                         open = "[]") {
  result <- character(0)
  if (length(x) >= ngram_size) {
    # -- skipgrams --
    if (max_skip > 0) {
      result <- skipgrams(x, ngram_size = ngram_size,
                          max_skip = max_skip, sep = sep) 
      # -- regular ngrams --
    } else {
      parts <- vector(mode = "list", length = ngram_size)
      for (i in 1:ngram_size) {
        parts[[i]] <- x[i:(i + length(x) - ngram_size)]
      }
      # -- without open slots --
      if (n_open == 0) {
        result <- list_paste(parts, sep = sep)
        # -- with open slots --
      } else {
        open_pos_list <- build_open_pos(ngram_size, n_open)
        for (open_pos in open_pos_list) {
          result <- c(result,
                      list_paste_open(parts,
                                      open_pos = open_pos,
                                      sep = sep,
                                      open = open))
        }
      }
    }
  }
  result
}

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

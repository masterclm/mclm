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
    return(character(0))
  }
  if (first_pos %in% open_pos) {
    car <- rep(open, length(x[[1]]))
  } else {
    car <- x[[1]]
  }
  
  if (length(x) == 1) {
    return(car)
  }
  
  paste(car,
        list_paste_open(x[-1], first_pos = first_pos + 1, sep = sep,
                        open_pos = open_pos, open = open),
        sep = sep)
}

#' Get indices of open positions
#' 
#' @param ngram_size Size of the n-gram
#' @param n_open Number of open slots
#' 
#' @return List of numeric vectors with open slot positions
#' @noRd
build_open_pos <- function(ngram_size, n_open) {
  result <- list()
  if ((n_open > 0) && (ngram_size - n_open > 1)) {
    result <- as.list(2:(ngram_size - n_open)) # get possible first slots
    n_open <- n_open - 1
  }
  if (length(result) == 0) {
    return(result)
  }
  
  while (n_open > 0) {
    result <- lapply(result, function(item) {
      last <- item[length(item)]
      lapply((last+1):(ngram_size-n_open), function(x) c(item, x))
    })
    result <- unlist(result, recursive = FALSE)
    n_open <- n_open - 1
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
  if (length(x) < ngram_size) {
    return(character(0))
  }
  
  if (max_skip > 0) {
    result <- skipgrams(x, ngram_size = ngram_size, max_skip = max_skip, sep = sep)
    return(result)
  }
  
  get_parts <- function(x, i, ngram_size) {
    x[i:(i + length(x) - ngram_size)]
  }
  parts <- lapply(seq(ngram_size), get_parts, x = x, ngram_size = ngram_size)
  
  if (n_open == 0) {
    return(list_paste(parts, sep = sep))
  }
  
  open_pos_list <- build_open_pos(ngram_size, n_open)
  if (length(open_pos_list) == 0) {
    return(character(0))
  }
  
  result <- lapply(open_pos_list, function(open_pos) {
    list_paste_open(parts, open_pos = open_pos, sep = sep, open = open)
  })
  unlist(result)
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

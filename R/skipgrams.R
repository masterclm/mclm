#' Create a new task with an empty set of items
#' 
#' @param i Position of the next item in the token sequence.
#' @param skips_left Number of remaining skips that can be used.
#' 
#' @return A named list with an `items` element containing an empty
#'   `dl_list` and elements for `i` and `skips_left`.
#' 
#' @noRd
make_new_task <- function(i, skips_left) {
  task <- new.env(parent = emptyenv())
  task$items <- dl_list()
  task$i <- i
  task$skips_left <- skips_left
  task
}

#' Make a new task of contents
#' 
#' @param x A task
#' @return A copy of `x`
#' @noRd
copy_task <- function(x) {
  copy <- make_new_task(i = x$i, skips_left = x$skips_left)
  old_x_cur <- x$items$current  
  goto_sentinel(x$items)
  while (goto_next(x$items)) {
    push(copy$items, get_cur(x$items))
  }
  x$items$current <- old_x_cur
  copy
}

#' Turn tokens of a skipgram into a character vector
#'
#' @param x Vector of [`tokens`]
#' @param y Object of class `dl_list` with positions
#' @param sep Separator to join the elements in `x` (`collapse` in [paste()])
#'
#' @return A character vector of length one
#' @noRd
items_to_character <- function(x, y, sep = "_") {
# x is a vector of tokens; y is a dl_list with positions
  if (get_size(y) == "0") {
    result <- ""
  } else {
    seq <- vector(mode = "list", length = get_size(y))
    i <- 0
    goto_sentinel(y)
    while (goto_next(y)) {
      i <- i + 1
      idx <- get_cur(y)
      seq[[i]] <- x[idx]
    }
    result <- paste0(seq, collapse = sep)
  }
  result
}

#' R implementation of skipgrams (not used)
#' 
#' @rdname skipgrams
#' @noRd
skipgrams_R <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  tasks     <- dl_list() # can be incomplete of complete skipgrams
  completed <- dl_list() # skipgrams that are established to be complete
  x_len     <- length(x)
  ## -- set initial tasks
  if ((x_len - ngram_size + 1) >= 1) {
    for (i in 1:(x_len - ngram_size + 1)) {
      new_task <- make_new_task(i = i + 1, skips_left = max_skip)
      push(new_task$items, i)
      push(tasks, new_task)
    }
  }
  ## -- the 'while loop' that processes tasks until 'tasks' is empty
  while (has_items(tasks)) {
    cur_task <- get_last(tasks)
    cur_n_items = get_size(cur_task$items) 
    ## -- if current task is a completed task (skipgram reached required size)
    if (cur_n_items == ngram_size) { 
      push(completed, pop(tasks))
    ## -- else if current task cannot be expanded upon (end x reached)
    } else if (cur_task$i > x_len) {  
      pop(tasks)
    ## -- else if current task can be expanded upon (there's more x)
    } else { 
      # first possible expansion branch: try skip i (requires:cur_n_items > 0)
      if (cur_task$skips_left > 0) {
        new_task <- copy_task(cur_task)
        new_task$i <- new_task$i + 1
        new_task$skips_left = new_task$skips_left - 1
        push(tasks, new_task)
      }
      # second expansion branch: add i to items (we re-use cur_task)
      push(cur_task$items, cur_task$i)
      cur_task$i <- cur_task$i + 1 
    }
  }
  ## build result
  seq <- vector(mode = "list", length = get_size(completed))
  i <- 0
  goto_sentinel(completed)
  while (goto_next(completed)) {
    i <- i + 1
    items <- get_cur(completed)$items
    seq[[i]] <- items_to_character(x, items, sep = sep)
  }
  unlist(seq)
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

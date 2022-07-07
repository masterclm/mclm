make_new_task <- function(i, skips_left) {
# new tasks are created with empty set of items
  task <- new.env(parent = emptyenv())
  task$items <- dl_list()
  task$i <- i
  task$skips_left <- skips_left
  task
}

copy_task <- function(x) {
# makes a new task the contents of which is a copy of the contents of x
  copy <- make_new_task(i = x$i, skips_left = x$skips_left)
  old_x_cur <- x$items$current  
  goto_sentinel(x$items)
  while (goto_next(x$items)) {
    push(copy$items, get_cur(x$items))
  }
  x$items$current <- old_x_cur
  copy
}

add_new_item <- function(x, y) {
# x is a task; y is an item; adds y to the items in x
  push(x$items, y)
  invisible(x$items)
}

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

skipgrams_orig <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  ## -------------------------------------------------------------------------
  ## arguments:
  ##   - x: a sequence of tokens
  ##   - n: requested size of skipgrams
  ##   - max_skip: maximum numbers of tokens to be skipped in skipgram
  ## -------------------------------------------------------------------------
  ## tasks in the stack have the following slots:
  ##   - items: list with positions of items already in the (partial) skipgram
  ##   - i: the position of the next item in the token sequence x
  ##   - skips_left: the number of remaining skips (that can be used) 
  ## -------------------------------------------------------------------------
  tasks     <- dl_list() # can be incomplete of complete skipgrams
  completed <- dl_list() # skipgrams that are established to be complete
  x_len     <- length(x)
  ## -- set initial task
  push(tasks, make_new_task(i = 1, skips_left = max_skip))
  ## -- the 'while loop' that processes tasks until 'tasks' is empty
  while (has_items(tasks)) {
    cur_task <- pop(tasks)
    cur_n_items = get_size(cur_task$items) 
    ## -- if current task is a completed task (skipgram reached required size)
    if (cur_n_items == ngram_size) { 
      push(completed, cur_task)
    ## -- else if current task can be expanded upon (because there's more x)
    } else if (cur_task$i <= x_len) { 
      # first possible expansion branch: don't start yet
      if (cur_n_items == 0) {
        new_task <- make_new_task(i = cur_task$i + 1, skips_left = max_skip)
        push(tasks, new_task) 
      # second possible expansion branch: try skip i (requires:cur_n_items > 0)
      } else if (cur_task$skips_left > 0) {
        new_task <- copy_task(cur_task)
        new_task$i <- new_task$i + 1
        new_task$skips_left = new_task$skips_left - 1
        push(tasks, new_task)
      }
      # third expansion branch: add i to items (we re-use cur_task)
      add_new_item(cur_task, cur_task$i)
      cur_task$i <- cur_task$i + 1
      push(tasks, cur_task)         # push modified cur_task on stack 
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

skipgrams_R <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  ## -------------------------------------------------------------------------
  ## version that is a bit more time efficient than skipgrams_orig
  ## -------------------------------------------------------------------------
  ## improvements [in decreasing order of importance]:
  ##  - larger set of initial tasks (and eliminate 'dont' start yet' branch
  ##  - avoid popping task and then pushing it again (as modified task)
  ##           -> i.e. keep task in as long pop is not unavoidable
  ##  - avoid call to add_new_item [doesn't help much]
  ## -------------------------------------------------------------------------
  ## arguments:
  ##   - x: a sequence of tokens
  ##   - n: requested size of skipgrams
  ##   - max_skip: maximum numbers of tokens to be skipped in skipgram
  ## -------------------------------------------------------------------------
  ## tasks in the stack have the following slots:
  ##   - items: list with positions of items already in the (partial) skipgram
  ##   - i: the position of the next item in the token sequence x
  ##   - skips_left: the number of remaining skips (that can be used) 
  ## -------------------------------------------------------------------------
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

skipgrams <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  ## -------------------------------------------------------------------------
  ## version that uses C++ (C++ code mimics skipgrams_orig()
  ## -------------------------------------------------------------------------
  ## arguments:
  ##   - x: a sequence of tokens
  ##   - n: requested size of skipgrams
  ##   - max_skip: maximum numbers of tokens to be skipped in skipgram
  ## -------------------------------------------------------------------------
  ## tasks in the stack have the following slots:
  ##   - items: list with positions of items already in the (partial) skipgram
  ##   - i: the position of the next item in the token sequence x
  ##   - skips_left: the number of remaining skips (that can be used) 
  ## -------------------------------------------------------------------------
  skipgramsC(x, ngram_size, max_skip, sep)
}

skipgrams_with_C_orig <- function(x, ngram_size = 3, max_skip = 2, sep = "_") {
  ## -------------------------------------------------------------------------
  ## version that uses C++ (C++ code mimics skipgrams_orig()
  ## -------------------------------------------------------------------------
  ## arguments:
  ##   - x: a sequence of tokens
  ##   - n: requested size of skipgrams
  ##   - max_skip: maximum numbers of tokens to be skipped in skipgram
  ## -------------------------------------------------------------------------
  ## tasks in the stack have the following slots:
  ##   - items: list with positions of items already in the (partial) skipgram
  ##   - i: the position of the next item in the token sequence x
  ##   - skips_left: the number of remaining skips (that can be used) 
  ## -------------------------------------------------------------------------
  pos_mat <- skipgramsC_orig(length(x), ngram_size, max_skip)
  unlist(lapply(1:nrow(pos_mat),
                function(i) paste0(x[pos_mat[i, ]],
                                   collapse = sep)))
}


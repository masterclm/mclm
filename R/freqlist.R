#' Interactively navigate through frequency list
#' 
#' This method only works in an interactive R session to open
#' 'exploration mode', in which the user can navigate through the \code{freqlist}
#' object \code{x} by means of brief commands. In 'exploration mode' the user can
#' ask of a list of available commands by keying in \code{?}, followed by ENTER.
#' The user can quiet 'exploration mode' by keying in \code{q}, followed by ENTER.
#'
#' @param x Object of class \code{freqlist}.
#' @param n Maximum number of items in the frequency list to be shown at once.
#' @param from First item in the frequency list that is shown at the beginning
#'   of the interactive session.
#' @param perl Boolean value. Whether regular expressions used in the
#'   exploration session use the PERL flavor of regular expression.
#' @param use_clear Boolean value. If \code{use_clear} is \code{TRUE},
#'   and if moreover the feature is supported by the R environment,
#'   the console will be cleared in between all interactive steps
#'   in the exploration session.
#' @param ... Additional arguments.
#'
#' @return Invisibly, \code{x}.
#' @exportS3Method explore freqlist
#' @export
explore.freqlist <- function(x,
                             n = 20,
                             from = 1,
                             perl = TRUE,
                             use_clear = TRUE,
                             ...) {
  if (interactive()) {
    length_x <- n_types(x)                     # n items in x
    cur_command <- "i"                         # "idle" (no change of state)
    cur_com_verb <- substr(cur_command, 1, 1)  # actual command 
    cur_regex <- ".*"                          # last regex that was used
    print_extra <- settings()                  # printing settings
    cur_hits <- numeric(0)                     # ids of hits for last regex
    while (cur_com_verb != "q") {
      ## -- initialize printing settings --
      assign("type_regex", NULL, envir = print_extra)
      ## -- prepare console --
      if (use_clear) clear_console()
      cat(mclm_style_dim(char_line())); cat("\n")
      ## -- process current instruction --
      if (cur_com_verb == "?") {           ## ? stand for 'help'
        cat(mclm_style_dim("?: show this help information\n"))
        cat(mclm_style_dim("b: go to the begin of the list\n"))
        cat(mclm_style_dim("e: go to the end of the list\n"))
        cat(mclm_style_dim("p: go to previous item (move up one item)\n"))
        cat(mclm_style_dim("n: go to next item (move down one item)\n"))
        cat(mclm_style_dim("u: move up n items\n"))
        cat(mclm_style_dim("d: move down n items\n"))
        cat(mclm_style_dim("g 123: go to item 123\n"))
        cat(mclm_style_dim("f regex: find next match for regex\n"))
        cat(mclm_style_dim("ENTER: back to list of items\n")) 
        cat(mclm_style_dim("q: quit explore mode\n"))
      } else {
        if (cur_com_verb == "e") {         ## e stands for '[e]nd of list'
          from <- max(1, length_x - n + 1)
        } else if (cur_com_verb == "b") {  ## b stand for '[b]egin of list'
          from <- 1
        } else if (cur_com_verb == "p") {  ## p stands from '[p]revious item'
          from <- max(1, from - 1)
        } else if (cur_com_verb == "n") {  ## n stands from '[n]ext item'
          from <- max(1, from + 1)
          from <- min(from, max(1, length_x - n + 1))
        } else if (cur_com_verb == "u") {  ## u stands for '[u]p one page'
          from <- max(1, from - n)
        } else if (cur_com_verb == "d") {  ## d stands for '[d]own one page'
          from <- max(1, from + n)
          from <- min(from, max(1, length_x - n + 1))
        } else if (cur_com_verb == "f") {  ## f stands for '[f]ind next match'
          f_arg <- ""
          old_regex <- cur_regex
          old_hits <- cur_hits
          tryCatch({
               f_arg <- cleanup_spaces(
                  substr(cur_command, 2, nchar(cur_command)))
               if (nchar(f_arg) == 0) {
                 cur_regex <- old_regex
               } else {
                 cur_regex <- f_arg
               }
               cur_hits <- grep(cur_regex, type_names(x), perl = perl)
             },
             error = function(e) {
               cur_regex <- old_regex
               cur_hits <- old_hits
             })
          tot_n_hits <- length(cur_hits)
          if (nchar(f_arg) == 0) {
            cur_hits <- cur_hits[cur_hits > from]
          } else {
            cur_hits <- cur_hits[cur_hits >= from]
          }
          pos_cur_hit <- tot_n_hits - length(cur_hits) + 1 
          if (length(cur_hits) > 0) {
             from <- cur_hits[1]
             assign("type_regex", cur_regex, envir = print_extra)
          } 
        } else if (cur_com_verb == "g") { ## g stands for '[g]o to item'
          old_from <- from
          tryCatch(from <- as.integer(substr(cur_command, 2,
                                             nchar(cur_command))),
                   error = function(e) from <- old_from)
          from <- max(1, min(from, length_x))
        }
        print(x, n = n, from = from, extra = print_extra, ...)
      }
      if (!is.null(print_extra$type_regex)) {
        cat(mclm_style_dim(paste0("search pattern: ", print_extra$type_regex, "\n")))
        cat(mclm_style_dim(paste0("<looking at matching item ", pos_cur_hit,
                            " out of ", tot_n_hits, " matching items>\n"))) 
      }
      cat(mclm_style_dim(char_line())); cat("\n")
      cat(mclm_style_dim("Enter command (? for help; q to quit explore mode) "))
      cur_command <- tolower(cleanup_spaces(readline(prompt = ">> ")))
      if (nchar(cur_command) == 0) {
        cur_com_verb <- "i"               ## i stands for [i]dle
      } else {
        cur_com_verb <- substr(cur_command, 1, 1)
      }
    }
  }
  invisible(x)
}

#' Give Number of Tokens in a 'freqlist' Object
#' 
#' Return the number of tokens in \code{x}.
#'
#' @param x Object of class \code{freqlist}.
#' @param ... Additional arguments.
#'
#' @return A number.
#' @exportS3Method n_tokens freqlist
#' @export
#'
#' @examples
#' (tks <- tokenize("The old man and the sea."))
#' n_tokens(tks)
#' 
#' (flist <- freqlist(tks))
#' n_tokens(flist)
#' n_types(flist)
n_tokens.freqlist <- function(x, ...) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  sum(x)
}  

#' Give Number of Types in a 'freqlist' Object
#' 
#' Return the number of types in \code{x}.
#'
#' @param x An object of class \code{freqlist}.
#' @param ... Additional arguments.
#'
#' @return A number.
#' @exportMethod n_types freqlist
#' @export
#'
#' @examples
#' (tks <- tokenize("The old man and the sea."))
#' n_tokens(tks)
#' 
#' (flist <- freqlist(tks))
#' n_tokens(flist)
#' n_types(flist)
n_types.freqlist <- function(x, ...) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  length(x)
}

type_names.freqlist <- function(x, ...) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  names(x)
}

type_freq <- function(x, types = NULL, with_names = FALSE, ...) {
  type_freqs(x, types = types, with_names = with_names, ...)
}

type_freqs <- function(x, types = NULL, with_names = FALSE, ...) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  if (is.null(types)) {
    result <- as.numeric(x)
    if (with_names) {
      names(result) <- type_names(x)
    }
  } else {
    mtch <- match(as.character(types), type_names(x))
    result <- as.numeric(x)[mtch]
    result[is.na(result)] <- 0
    if (with_names) {
      names(result) <- types
    }    
  }
  result
}


# public function freqlist()
# build a 'freqlist' object on the basis of the texts in x
#  x is a character vector that either
#   (i)  contains the filenames of the corpus files (if as_text is FALSE), or
#   (ii) contains the actual textual data           (if as_text is TRUE)
freqlist <- function(x,
                     re_drop_line = NULL,
                     line_glue = NULL, 
                     re_cut_area = NULL,
                     re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
                     re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
                     re_drop_token = NULL,
                     re_token_transf_in = NULL,
                     token_transf_out = NULL,
                     token_to_lower = TRUE,
                     perl = TRUE,
                     blocksize = 300,
                     verbose = FALSE,
                     show_dots = FALSE,
                     dot_blocksize = 10,
                     file_encoding = "UTF-8",
                     ngram_size = NULL,
                     max_skip = 0,
                     ngram_sep = "_",
                     ngram_n_open = 0,
                     ngram_open = "[]",
                     as_text = FALSE) {
  if (is.null(x)) {
    x <- ""
    as_text <- TRUE
  }
  if ("types" %in% class(x)) {
    x <- as_tokens(x)
  }
  if (!"tokens" %in% class(x)) {
    x <- x[!is.na(x)] 
  }
  if ((length(x) == 0) || (length(x) == 1 && x[1] == "")) {
    x <- ""
    as_text <- TRUE    
  }
  if (("tokens" %in% class(x)) || as_text) {
    freqlist_char(x,
                  re_drop_line = re_drop_line,
                  line_glue = line_glue,
                  re_cut_area = re_cut_area,
                  re_token_splitter = re_token_splitter,
                  re_token_extractor = re_token_extractor,
                  re_drop_token = re_drop_token,
                  re_token_transf_in = re_token_transf_in,
                  token_transf_out = token_transf_out,
                  token_to_lower = token_to_lower,
                  perl = perl,
                  ngram_size = ngram_size,
                  max_skip = max_skip,
                  ngram_sep = ngram_sep,
                  ngram_n_open = ngram_n_open,
                  ngram_open = ngram_open)
  } else {
    freqlist_corp(x,
                  re_drop_line = re_drop_line,
                  line_glue = line_glue,
                  re_cut_area = re_cut_area,
                  re_token_splitter = re_token_splitter,
                  re_token_extractor = re_token_extractor,
                  re_drop_token = re_drop_token,
                  re_token_transf_in = re_token_transf_in,
                  token_transf_out = token_transf_out,
                  token_to_lower = token_to_lower,
                  perl = perl,
                  blocksize = blocksize,
                  verbose = verbose,
                  show_dots = show_dots,
                  dot_blocksize = dot_blocksize,
                  file_encoding = file_encoding,
                  ngram_size = ngram_size,
                  max_skip = max_skip,
                  ngram_sep = ngram_sep,
                  ngram_n_open = ngram_n_open,
                  ngram_open = ngram_open)
  }
}

# public function freqlist_char()
# build a 'freqlist' object on the basis of the texts in x
#  x is a character vector that 
#   - contains the actual textual data   
freqlist_char <- function(x,
                          re_drop_line = NULL,
                          line_glue = NULL, 
                          re_cut_area = NULL,
                          re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
                          re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
                          re_drop_token = NULL,
                          re_token_transf_in = NULL,
                          token_transf_out = NULL,
                          token_to_lower = TRUE,
                          perl = TRUE,
                          ngram_size = NULL,
                          max_skip = 0,
                          ngram_sep = "_",
                          ngram_n_open = 0,
                          ngram_open = "[]") {
  if ("tokens" %in% class(x)) {
    freqlist <- table(x) # in this case ngram_size is ignored
  } else {
    freqlist <-    table(tokenize(x,
                                  re_drop_line = re_drop_line,
                                  line_glue = line_glue,
                                  re_cut_area = re_cut_area,
                                  re_token_splitter = re_token_splitter,
                                  re_token_extractor = re_token_extractor,
                                  re_drop_token = re_drop_token,
                                  re_token_transf_in = re_token_transf_in,
                                  token_transf_out = token_transf_out,
                                  token_to_lower = token_to_lower,
                                  perl = perl,
                                  ngram_size = ngram_size,
                                  max_skip = max_skip,
                                  ngram_sep = ngram_sep,
                                  ngram_n_open = ngram_n_open,
                                  ngram_open = ngram_open))
  }
  as_freqlist(freqlist) # sets class and attributes
                        # and sorts types by rank  
}


cleanup_spaces <- function(x) {
  gsub("^\\s+|\\s+$", "", x, perl = TRUE)
}

x_freqlist <- function(x) {
  command <- "b"
  while (command != "q") {
    cat("blabla\n")
    cat("----")
    cat("[b = begin] | [e = end] | [q = quit] > ")
    command <- scan(what = "char", nmax = 1, quiet = TRUE)
    command <- cleanup_spaces(tolower(command))
  }
}

# public function freqlist_merge()
# merge two frequency lists, adding up the frequencies
# ---------------------------------------------------------------------------
# in the current implementation, orig_ranks are lost when merging
# ---------------------------------------------------------------------------
freqlist_merge <- function(x, y) {
  if ((!"freqlist" %in% class(x)) || (!"freqlist" %in% class(y))) {
    stop("both x and y must be of the class 'freqlist'")
  }
  as_freqlist(freqlist_merge_two(x, y)) # as_freqlist sorts types by rank
}  

# public function freqlist_merge_all()
# merge two or more frequency lists, adding up the frequencies 
# ---------------------------------------------------------------------------
# in the current implementation, orig_ranks are lost when merging
# ---------------------------------------------------------------------------
freqlist_merge_all <- function(...) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car ----------------------------
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("freqlist" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("freqlist_merge_all", car)
    }
  }   
  # -- processing cdr ----------------------------
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("freqlist_merge_all", cdr)
  }
  # -- merge results if needed -------------------
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- freqlist_merge_two(result_car, result_cdr)
  }
  # -- result ------------------------------------
  as_freqlist(result) # as_freqlist sorts types by rank
}

# public function freqlist_diff(x, y) 
# the result is a freqlist with the y frequencies subtracted from the x frequencies
freqlist_diff <- function(x, y) {
  names <- dplyr::union(names(x), names(y))
  tot_n_tokens_x <- attr(x, "tot_n_tokens")
  tot_n_tokens_y <- attr(y, "tot_n_tokens")
  # we avoid x[names] (and y[names]) because it is painfully slow
  # so we use the far more efficient x[match(names, names(x))] etc.
  x_new <- as.numeric(x)[match(names, names(x))]; x_new[is.na(x_new)] <- 0
  y_new <- as.numeric(y)[match(names, names(y))]; y_new[is.na(y_new)] <- 0
  result <- pmax(x_new - y_new, 0)
  names(result) <- names
  class(result) <- class(x) <- c("freqlist", "table")
  attr(result, "tot_n_tokens") <- max(tot_n_tokens_x - tot_n_tokens_y, 0)
  attr(result, "orig_ranks") <- NULL  
  as_freqlist(result)
}


# private function freqlist_merge_two()
# both x and y are assumed to be of class "freqlist"
# --------------------------------------------------------------------------
# WARNING! This private function does not sort the types in the result by rank,
#          so this is something the public functions freqlist_merge() and
#          freqlist_merge_all() need to take care of !!!
# ---------------------------------------------------------------------------
# in the current implementation, orig_ranks are lost when merging, because
# they are no longer necessarily unique.
# ---------------------------------------------------------------------------
freqlist_merge_two <- function(x, y) {
  names <- dplyr::union(names(x), names(y))
  tot_n_tokens_x <- attr(x, "tot_n_tokens")
  tot_n_tokens_y <- attr(y, "tot_n_tokens")
  # we avoid x[names] (and y[names]) because it is painfully slow
  # so we use the far more efficient x[match(names, names(x))] etc.
  x_new <- as.numeric(x)[match(names, names(x))]; x_new[is.na(x_new)] <- 0
  y_new <- as.numeric(y)[match(names, names(y))]; y_new[is.na(y_new)] <- 0
  result <- x_new + y_new
  names(result) <- names
  class(result) <- class(x) <- c("freqlist", "table")
  attr(result, "tot_n_tokens") <- tot_n_tokens_x + tot_n_tokens_y
  attr(result, "orig_ranks") <- NULL  
  result
}



# build a 'freqlist' object on the basis of the texts in x
#  x is a character vector that 
#   (i)  contains the filenames of the corpus files
freqlist_corp <- function(x,
                          re_drop_line = NULL,
                          line_glue = NULL, # e.g. "\n" or ""
                          re_cut_area = NULL,
                          re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
                          re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
                          re_drop_token = NULL,
                          re_token_transf_in = NULL,
                          token_transf_out = NULL,
                          token_to_lower = TRUE,
                          perl = TRUE,
                          blocksize = 300,
                          verbose = FALSE,
                          show_dots = FALSE,
                          dot_blocksize = 10,
                          file_encoding = "UTF-8",
                          ngram_size = NULL,
                          max_skip = 0,
                          ngram_sep = "_",
                          ngram_n_open = 0,
                          ngram_open = "[]") {
  if (verbose) {
    first_pt <- proc.time()
    new_pt <- first_pt
  }
  n_texts <- length(x)
  if (length(file_encoding) < n_texts) {
    file_encoding <- rep(file_encoding, length = n_texts)
  }   
  globfreqlist <- NULL
  i = 1
  while (i <= length(x)) {
    j = 0
    blocktokens <- vector(mode = "list", length = 10)
    while ((j < blocksize) && ((i + j) <= length(x))) {
      fname <- x[i + j]
      # -- read corpus file
      newlines <- readr::read_lines(
        fname,
        locale = readr::locale(encoding = file_encoding[i + j]))    
      # -- tokenize
      blocktokens[[j + 1]] <- tokenize(
        newlines,
        re_drop_line = re_drop_line,
        line_glue = line_glue,
        re_cut_area = re_cut_area,
        re_token_splitter = re_token_splitter,
        re_token_extractor = re_token_extractor,
        re_drop_token = re_drop_token,
        re_token_transf_in = re_token_transf_in,
        token_transf_out = token_transf_out,
        token_to_lower = token_to_lower,
        perl = perl,
        ngram_size = ngram_size,
        max_skip = max_skip,
        ngram_sep = ngram_sep,
        ngram_n_open = ngram_n_open,
        ngram_open = ngram_open)
      # ====================================================
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    blockfreqlist <- freqlist(tokens_merge_all(blocktokens))
    # alternative: ... <- freqlist(as_tokens(unlist(blocktokens))
    if (is.null(globfreqlist)) {
      globfreqlist <- blockfreqlist
    } else {
      globfreqlist <- freqlist_merge_two(globfreqlist, blockfreqlist)
    }
    if (verbose) {
      prev_pt <- new_pt
      new_pt <- proc.time()
      cat((i+j)-1,"(", new_pt[3]-first_pt[3], "|", 
          new_pt[3]-prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  if (verbose) cat("\n")
  as_freqlist(globfreqlist) # sets class and attributes if needed
  # and sorts by ranks 
}

# public S3 setter function "tot_n_tokens<-" for freqlist
`tot_n_tokens<-.freqlist` <- function(x, value) {
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")
  if (is.null(value)) stop("value must not be NULL")
  if (length(value) == 0) stop("length of value must not be zero")
  if (length(value) > 1) warning("value too long; only value[1] is used")
  if (is.na(value[1])) stop("value must not be NA")
  if (!is.numeric(value[1])) stop("value must be numeric")
  value <- round(value[1])
  if (value < 0) stop("value must not be negative")
  attr(x, 'tot_n_tokens') <- value
  x
}

# public S3 getter function tot_n_tokens for freqlist
tot_n_tokens.freqlist <- function(x) {
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")
  attr(x, 'tot_n_tokens')
}

# public S3 setter function "orig_ranks<-" for freqlist
`orig_ranks<-.freqlist` <- function(x, value) {
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")  
  if (!is.null(value)) stop("value must be NULL")
  attr(x, 'orig_ranks') <- value
  x
}

# public S3 getter function orig_ranks for freqlist
orig_ranks.freqlist <- function(x, with_names = FALSE, ...) {
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")
  result <- attr(x, 'orig_ranks')
  if (!is.null(result) && with_names) {
    names(result) <- type_names(x)
  }
  result
}

# public S3 getter function ranks.freqlist()
ranks.freqlist <- function(x, with_names = FALSE, ...) {
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")
  # ranks are by decreasing frequency first, and by alphabetic order
  # for the frequency ties
  freqs <- type_freqs(x)
  names <- type_names(x)
  # sort freqs and names alphabetically
  sort_idx <- order(names)
  freqs_sorted <- freqs[sort_idx]
  names_sorted <- names[sort_idx]
  # calculate ranks for sorted data
  ranks_sorted <- rank(-freqs_sorted, ties.method = "first")
  # translate result back to original order
  translate_idx <- match(names, names_sorted)
  result <- ranks_sorted[translate_idx]
  if (with_names) {
    names(result) <- names
  }
  # return result
  result
}

# public S3 method sort.freqlist()
sort.freqlist <- function(x,
                          decreasing = FALSE,
                          sort_crit = c("ranks", "names",
                                        "orig_ranks", "freqs"),
                          na_last = TRUE,
                          ...) {
  # testing x argument
  if (!"freqlist" %in% class(x)) stop("x must be of class 'freqlist'")  
  # testing decreasing argument
  if (is.null(decreasing)) stop("decreasing must not be NULL")
  if (is.na(decreasing[1])) stop("decreasing[1] must not be NA")
  if (!is.logical(decreasing[1])) stop("decreasing[1] must be TRUE or FALSE")
  # testing sort_crit argument
  if (is.null(sort_crit)) stop("sort_crit must not be NULL")
  if (!sort_crit[1] %in% c("ranks", "names", "orig_ranks", "freqs")) {
    stop("unsupported sort_crit")
  }
  # testing ... argument
  ext_args <- names(list(...))
  if ("na.last" %in% ext_args) {
    warning("na.last argument will be ignored; use na_last instead")
  }
  # sort frequency list 
  if (sort_crit[1] == "ranks") {
    idx <- order(ranks(x))
  } else if (sort_crit[1] == "freqs") {
    idx <- order(ranks(x), decreasing = TRUE)
  } else if (sort_crit[1] == "names") {
    names <- type_names(x)
    srt_names <- sort(names)
    idx <- match(srt_names, names)
  } else if (sort_crit[1] == "orig_ranks") {
    if (is.null(orig_ranks(x))) {
      idx <- order(ranks(x))
    } else {
      if (decreasing) { # anticipate that rev() will mess up na_last
        na_last <- !na_last
      }
      idx <- order(rank(orig_ranks(x),
                        ties.method = "first",
                        na.last = "keep"),
                   na.last = na_last)
    }
  }
  result <- x[idx]
  if (decreasing) {
    result <- rev(result)
  }
  result
}


# public function as_freqlist()
as_freqlist <- function(x, tot_n_tokens = NULL, sort_by_ranks = TRUE) {
  result <- x
  if (is.null(result)) {
    result <- vector(mode = "integer", length = 0)
  }
  if ((("numeric" %in% class(result)) || ("integer" %in% class(result))) &&
      (!is.null(names(result)))) {
    result <- as.table(result)
  }
  if (!"table" %in% class(result)) {
    stop("x must be a 'table' or a named numeric vector")
  }
  if (!"freqlist" %in% class(result)) {
    class(result) <- c("freqlist", "table")
  }
  # --- sorting by ranks -----------------------------------------------------
  # this must be done before we assign classes and attributes, because
  # result <- result[ord] loses such information
  # -------------------------------------------------------------------------- 
  if ((length(result) > 0) && sort_by_ranks) {
    result_orig_ranks <- attr(result, "orig_ranks")
    result_tot_n_tokens <- attr(result, "tot_n_tokens")
    ord <- order(ranks(result))
    class(result) <- "table" # prepare for next instruction
    result <- result[ord] # at this point, result must not be a "freqlist"
    class(result) <- c("freqlist", "table") # restore class
    if (!is.null(result_orig_ranks)) {
      attr(result, "orig_ranks") <- result_orig_ranks[ord]
    }
    if (!is.null(result_tot_n_tokens)) {
      attr(result, "tot_n_tokens") <- result_tot_n_tokens
    }
  }  
  # --------------------------------------------------------------------------
  # assign classes and attributes etc.
  # --------------------------------------------------------------------------
  if (!"freqlist" %in% class(result)) {
    class(result) <- c("freqlist", "table")
  }
  if (is.null(attr(result, "tot_n_tokens"))) {
    attr(result, "tot_n_tokens") <- sum(result)
  }
  if (!is.null(tot_n_tokens) && !is.na(tot_n_tokens[1])) {
    tot_n_tokens(result) <- tot_n_tokens[1]
  }
  # -------------------------------------------------------------------------
  result
}

sort2.freqlist <- function(x,
                          decreasing = FALSE,
                          na.last = NA,
                          sort_crit = c("rank", "alpha", "orig_rank"),
                          ...) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }  
  result <- x
  if (length(result) > 0) {
    if (sort_crit[1] == "rank") {
      ord <- order(ranks(result), decreasing = decreasing,
                   na.last = na.last, ...)
    } else if (sort_crit[1] == "alpha") {
      ord <- order(names(result), decreasing = decreasing,
                   na.last = na.last, ...)
    } else if (sort_crit[1] == "orig_ranks" &&
               !is.null(attr(result, "orig_ranks"))) {
      ord <- order(attr(result, "orig_ranks"), decreasing = decreasing,
                   na.last = na.last, ...)
    } else {
      ord <- 1:length(result)
    }
    class(result) <- "table"
    result <- result[ord]
    if (!is.null(attr(x, "orig_ranks"))) {
      attr(result, "orig_ranks") <- attr(x, "orig_ranks")[ord]
    }
    if (!is.null(attr(x, "tot_n_tokens"))) {
      attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
    }
    class(result) <- c("freqlist", "table")
  }
  result
}


# public S3 function as.data.frame()
as.data.frame.freqlist <- function(x,
                                   row.names = NULL,
                                   optional = FALSE,
                                   ...) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  ranks <- ranks(x)
  result <- list(rank = as.numeric(ranks))
  if (!is.null(attr(x, "orig_ranks"))) {
    result$orig_rank <- as.numeric(attr(x, "orig_ranks"))
  }
  result$type <- names(x)
  result$abs_freq <- as.numeric(x)
  if (tot_n_tokens(x) > 0) {
    result$nrm_freq <- (result$abs_freq / tot_n_tokens(x)) * 10000
  }
  # Hadley Wickham's efficient as.data.frame()
  class(result) <- "data.frame"
  attr(result, "row.names") <- .set_row_names(length(result[[1]]))
  # -- return result
  result
}

# public S3 function as_tibble()
as_tibble.freqlist <- function(x, ...) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  ranks <- ranks(x)
  result <- tibble(rank = as.numeric(ranks))
  if (!is.null(attr(x, "orig_ranks"))) {
    result <- mutate(result, 
                     orig_rank = as.numeric(attr(x, "orig_ranks")))
  }
  result <- mutate(result,
                   type = names(x),
                   abs_freq = as.numeric(x))
  if (tot_n_tokens(x) > 0) {
    result <- mutate(result,
                     nrm_freq = (as.numeric(x) / tot_n_tokens(x)) * 10000)
  }
  # -- return result
  result
}

# public S3 function plot()
plot.freqlist <- function(x, ...) {
  warning("'freqlist' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# public S3 function summary()
summary.freqlist <- function(object, ...) {
  if (! "freqlist" %in% class(object)) {
    stop("argument 'object' must be of the class 'freqlist'")
  }
  result <- list()
  result$n_types <- n_types(object)
  result$n_tokens <- n_tokens(object)
  result$tot_n_tokens <- tot_n_tokens(object)
  class(result) <- "summary.freqlist"
  result
}

# public S3 function summary()
print.summary.freqlist <- function(x, ...) {
  if (!"summary.freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'summary.freqlist'")
  }
  cat("Frequency list (types in list: ",
      x$n_types,
      ", tokens in list: ",
      x$n_tokens,
      ")\n",
      sep = "")
  if (x$n_tokens != x$tot_n_tokens) {
    cat("<total number of tokens: ",
         x$tot_n_tokens,
        ">\n",
        sep = "")  
  }  
  invisible(x)
}


# public S3 function plot()
plot.summary.freqlist <- function(x, ...) {
  warning("'summary.freqlist' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# public S3 function print()
print.freqlist <- function(x,
                           n = 20, from = 1,
                           extra = NULL,
                           ...) {
  # testing and processing argument 'x'
  if (!"freqlist" %in% class(x)) {
    stop("x must be of the class 'freqlist'")
  }
  n_types <- length(x)
  n_tokens <- sum(x)
  tot_n_tokens <- tot_n_tokens(x)
  # testing and processing argument 'n'
  if (length(n) == 0) {
    stop("n must be a numeric vector of length one")
  } else if (length(n) > 1) {
    n <- n[1]
    warning("only using n[1] instead of the whole of n")
  } 
  if (is.na(n) || !is.numeric(n)) {
    stop("inappropriate value for n")
  }
  n <- max(0, round(n))
  # testing and processing argument 'from'
  if (length(from) == 0) {
    stop("from must be a numeric vector of length one")
  } else if (length(from) > 1) {
    from <- from[1]
    warning("only using from[1] instead of the whole of from")
  } 
  if (is.na(from) || !is.numeric(from)) {
    stop("inappropriate value for from")
  }
  from <- max(1, round(from))
  # adjusting 'n' to 'from'
  n <- max(0, min(n, n_types - from + 1))
  if (n > 0) {
    all_ranks <- ranks(x)
    all_orig_ranks <- attr(x, "orig_ranks")
    ord <- from:(from + n - 1)
  }
  # testing argument 'extra'
  if (!is.null(extra) && !is.environment(extra)) {
    stop("incorrect use of the argument 'extra'")
  }
  # printing 'x'
  cat(mclm_style_dim(paste0(
    "Frequency list (types in list: ",
    n_types,
    ", tokens in list: ",
    n_tokens,
    ")\n")))
  if (n_tokens != tot_n_tokens) {
    cat(mclm_style_dim(paste0(
      "<total number of tokens: ",
      tot_n_tokens,
      ">\n")))  
  }  
  if (n > 0) {
    ranks <- all_ranks[ord]
    if (!is.null(all_orig_ranks)) orig_ranks <- all_orig_ranks[ord]
    types <- names(x)[ord]
    freqs <- as.numeric(x)[ord]
    if (tot_n_tokens(x) > 0) {
      nrm_freqs <- round((freqs / tot_n_tokens) * 10000, digits = 3)
    }
    format_ranks <- format(c("rank", 
                             format(ranks,
                                    scientify = FALSE, 
                                    justify = "right")), 
                           justify = "right")
    if (!is.null(all_orig_ranks)) {
      format_orig_ranks <- format(c("orig_rank", 
                                    format(orig_ranks,
                                           scientify = FALSE, 
                                           justify = "right")), 
                                  justify = "right")
    }
    # we don't use format() for types [problems with unicode !]
    # nor do we use stringi::stri_pad_left [hickups with greek and Set.locale]
    ## -- begin of apply regex, if any --
    nchar_types <- nchar(types)
    if (!is.null(extra$type_regex)) {
      types <- show_matches(types, extra$type_regex)
    }    
    format_types <- mclm_pad_left(
                      c("type", types),
                      max(nchar("type"), nchar_types),
                      nchar_x = c(nchar("type"), nchar_types))
    ## -- end of apply regex, if any --
    format_freqs <- format(c("abs_freq", 
                             format(freqs, scientify = FALSE, 
                                    justify = "right")), 
                           justify = "right")
    if (tot_n_tokens(x) > 0) {
      format_nrm_freqs <- format(c("nrm_freq", 
                                   format(nrm_freqs,
                                          scientify = FALSE, 
                                          justify = "right")), 
                                 justify = "right")
    }
    # -- print titles
    cat(format_ranks[1], " ", sep = "")
    if (!is.null(all_orig_ranks)) cat(format_orig_ranks[1], " ", sep = "")
    cat(format_types[1],
        format_freqs[1],
        sep = " ")
    if (tot_n_tokens(x) > 0) {
       cat(" ", format_nrm_freqs[1], sep = "")
    }
    cat("\n")
    # -- print horizontal lines
    cat(rep_len("-", nchar(format_ranks[1])), " ", sep = "")
    if (!is.null(all_orig_ranks)) {
      cat(rep_len("-", nchar(format_orig_ranks[1])), " ", sep = "")
    }
    cat(rep_len("-", nchar(format_types[1])),
        " ",
        rep_len("-", nchar(format_freqs[1])),
        sep = "")
    if (tot_n_tokens(x) > 0) {
       cat(" ", rep_len("-", nchar(format_nrm_freqs[1])), sep = "")
    }
    cat("\n")
    # -- optionally print dots
    if (from > 1) cat(mclm_style_very_dim("...\n"))
    # -- print items  
    for (j in seq_along(ord)) {
      cat(format_ranks[j + 1], " ", sep = "")
      if (!is.null(all_orig_ranks)) cat(format_orig_ranks[j + 1], " ", sep = "")      
      cat(format_types[j + 1],
          format_freqs[j + 1],
          sep = " ")
      if (tot_n_tokens(x) > 0) {
         cat(" ", format_nrm_freqs[j + 1], sep = "")
      }
      cat("\n")
    }
    # -- optionally print dots
    if ((from + n - 1) < n_types) cat(mclm_style_very_dim("...\n"))
  }
  invisible(x)
}

# public S3 function drop_types()
drop_types.freqlist <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.freqlist(x, types, invert = TRUE, ...)
}

# public S3 function keep_types()
keep_types.freqlist <- function(x, types, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  # -- test and process argument 'types'
  types <- as.character(types) # turns NULL into character(0)
  types <- types[!is.na(types)]
  # -- test and process argument 'invert' !
  if (is.null(invert)) {
    stop("invert must not be NULL")    
  }
  if (!is.logical(invert) || is.na(invert[1])) {
    stop("invert must either be TRUE or FALSE")
  }  
  # -- build result  
  if (length(x) == 0) {
    result <- x
  } else {  
    # prepare creation of result
    mtch <- match(types, names(x)) # we avoid x_ranks[types] and x[types]
    if (invert) {
      mtch <- setdiff(1:length(x), mtch)
    }
    if (length(mtch) == 0) {
      result <- freqlist("", as_text = TRUE)
      attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
    } else if (invert) { # simple case, no doubles or missing cases
      result <- subset_freqlist(x, mtch)
    } else { # more complicated case, can contain doubles or missing cases
      tot_n_tokens <- attr(x, "tot_n_tokens")
      x_ranks <- attr(x, "orig_ranks")
      if (is.null(x_ranks)) x_ranks <- ranks(x)
      result_orig_ranks <- x_ranks[mtch]
      # create result
      result <- type_freqs(x)[mtch]
      result[is.na(result)] <- 0
      names(result) <- types # assigns clean names (cf. missing/double cases)
      class(result) <- c("freqlist", "table")  
      attr(result, "tot_n_tokens") <- tot_n_tokens
      attr(result, "orig_ranks") <- result_orig_ranks
    }
  }
  # return result
  result
}

# public S3 function drop_re()
drop_re.freqlist <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.freqlist(x, pattern, perl = perl, invert = TRUE, ...)
}


# public S3 function keep_re()
keep_re.freqlist <- function(x, pattern, perl = TRUE, invert = FALSE, ...) {
  # -- test x for errors
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  # -- test pattern for errors (and process pattern if it's an 're' object)
  if ("re" %in% class(pattern)) {
    perl <- perl_flavor(pattern)  # perl_flavor(pattern) overrules perl
    pattern <- as_character(pattern)
  }
  if (!"character" %in% class(pattern)) {
    stop("pattern must be an 're' object or a character vector")
  }
  if (is.na(pattern[1])) {
    stop("pattern[1] must not be NA")
  }  
  # -- test perl for errors
  if (!is.logical(perl)) {
    stop("perl must be a logical vector")
  }
  if (is.na(perl[1])) {
    stop("perl[1] must not be NA")
  }  
  # -- test invert for errors
  if (!is.logical(invert)) {
    stop("invert must be a logical vector")
  }
  if (is.na(invert[1])) {
    stop("invert[1] must not be NA")
  }
  # -- test pattern for warnings
  if (length(pattern) > 1) {
    warning("pattern contains multiple items; only pattern[1] is used")
  }
  # -- test perl for warnings
  if (length(perl) > 1) {
    warning("perl contains multiple items; only perl[1] is used")
  }
  # -- test invert for warnings
  if (length(invert) > 1) {
    warning("invert contains multiple items; only invert[1] is used")
  }
  # build result
  if (n_types(x) == 0) {
    result <- x
  } else {  
    # prepare creation of result
    sel <- grep(pattern[1], type_names(x), perl = perl[1], invert = invert[1])
    if (length(sel) == 0) {
      result <- freqlist("", as_text = TRUE)
      attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
    } else {
      result <- subset_freqlist(x, sel)
    }
  }
  # return result
  result
}

# public S3 function drop_bool()
drop_bool.freqlist <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.freqlist(x, !bool, ...)
}

# public S3 function keep_bool()
keep_bool.freqlist <- function(x, bool, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  # -- test and process argument 'bool'
  if (is.null(bool)) stop("bool must not be NULL")
  if (!is.logical(bool)) stop("bool must be a logical vector")
  if (any(is.na(bool))) stop("bool must not contain NAs")
  if (length(x) != length(bool)) bool <- rep_len(bool, length(x))
  # -- test and process argument 'invert' !
  if (is.null(invert)) {
    stop("invert must not be NULL")    
  }
  if (!is.logical(invert) || is.na(invert[1])) {
    stop("invert must either be TRUE or FALSE")
  }
  if (length(invert) > 1) {
    warning("invert contains multiple items; only invert[1] is used")
  }
  if (invert[1]) bool <- !bool
  # -- build result
  if (n_types(x) == 0) {
    result <- x
  } else if (sum(bool) == 0) {
    result <- freqlist("", as_text = TRUE)
    attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
  } else {  
    result <- subset_freqlist(x, bool)
  }
  # -- return result
  result
}

# public S3 function drop_pos()
drop_pos.freqlist <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.freqlist(x, pos, invert = TRUE, ...)
}

# public S3 function keep_pos()
keep_pos.freqlist <- function(x, pos, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class 'freqlist'")
  }
  # -- test and process argument 'pos'
  if (missing(pos) || is.null(pos)) {
    pos <- vector(mode = "numeric", length = 0)
  }
  if (! (is.numeric(pos) || is.integer(pos))) {
    stop("pos must be of class 'numeric' or 'integer'")
  }
  # -- test and process argument 'invert' !
  if (is.null(invert)) {
    stop("invert must not be NULL")    
  }
  if (!is.logical(invert) || is.na(invert[1])) {
    stop("invert must either be TRUE or FALSE")
  }  
  # -- build result 
  if (n_types(x) == 0) {
    result <- x
  } else {
    pos <- trunc(pos)
    any_pos <- any(pos >= 1)
    any_neg <- any(pos <= -1)
    if (any_pos && any_neg) {
      stop("values in pos must be either all positive or all negative")          
    }
    if (any_neg) {
      invert <- !invert
      pos <- abs(pos)
    }
    mtch <- pos[pos >= 1 & pos <= length(x)]
    mtch <- mtch[!is.na(mtch)] # remove NAs
    if (invert) {
      mtch <- setdiff(1:length(x), mtch)
    }
    result <- subset_freqlist(x, mtch)
  }
  # -- return result
  result
}

# private subset selection function
# x is assumed to be a freqlist object (not tested)
# sel can be:
#  - numeric vector with positions
#  - boolean vector
subset_freqlist <- function(x, sel) {
  result <- as.numeric(x)[sel]
  names(result) <- names(x)[sel]
  attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
  new_orig_ranks <- attr(x, "orig_ranks")
  if (is.null(new_orig_ranks)) new_orig_ranks <- ranks(x)
  attr(result, "orig_ranks") <- new_orig_ranks[sel]
  class(result) <- c("freqlist", "table")
  result
}


# public function read_freqlist()
#  - reads a 'freqlist' object from a csv file
#  - by default also tries to read the associated config file
read_freqlist <- function(file,
                          sep = "\t",
                          file_encoding = "UTF-8",
                          ...) {
  lines <- readr::read_lines(
             file,
             locale = readr::locale(encoding = file_encoding))
  lines <- lines[nchar(lines) > 0]          # drop empty lines
  lines <- lines[-1]                        # drop header line
  cells <- strsplit(lines, sep)
  types <- unlist(lapply(cells, "[", 1))
  result <- as.numeric(unlist(lapply(cells, "[", 2)))
  names(result) <- types
  result <- as_freqlist(result)
  config <- read_config(file)
  if (!is.null(config$tot_n_tokens)) {
    attr(result, "tot_n_tokens") <- config$tot_n_tokens
  } 
  result
}

# public function write_freqlist()
#  - writes a 'freqlist' object to a csv file
#  - by default also creates an associated config file
# ------------------------------------------------------
# in the current implementation, orig_ranks are not
# written to file
# ------------------------------------------------------
write_freqlist <- function(x,
                           file,
                           sep = "\t",
                           make_config_file = TRUE,
                           ...) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  readr::write_lines(paste0("type", sep, "frequency"), file)
  lines <- paste0(names(x), sep, x)
  readr::write_lines(lines, file, append = TRUE)
  if (make_config_file) {
    config <- list(data_class = "freqlist",
                   csv_header = "TRUE",
                   csv_sep = "\\t",
                   csv_quote = "",
                   csv_comment_char = "",
                   tot_n_tokens = attr(x, "tot_n_tokens"))
    write_config(config, file)
  }
  invisible(x)
}

rev.freqlist <- function(x) {
  if (! "freqlist" %in% class(x)) {
    stop("argument 'x' must be of the class 'freqlist'")
  }
  nt <- n_types(x)
  if (nt == 0) {
    result <- x
  } else {
    result <- x[nt:1]
  }
  result
}

`[.freqlist` <- function(x, i, invert = FALSE, ...) {
  if (!"freqlist" %in% class(x)) {
    stop("subsetted object must be of class 'freqlist'")
  }
  result <- x
  if (!is.null(x) && !missing(i) && !is.null(i)) {
    if (any(is.na(i))) {
      stop("subset criterion must not contain any NAs")
    }    
    if (is.numeric(i) || is.integer(i)) {
      i <- i[!is.na(i)]
      if (length(i) > 0) {
        i <- trunc(i)
        any_pos <- any(i >= 1)
        any_neg <- any(i <= -1)
        if (any_pos && any_neg) {
          stop("subsetting indices must be either all positive or all negative")          
        }
        if (any_neg) {
          invert <- !invert
          i <- abs(i)
        }
        result <- keep_pos(x, i, invert = invert, ...)
      } 
    } else if ("types" %in% class(i)) {
      result <- keep_types(x, i, invert = invert, ...)
    } else if ("character" %in% class(i)) {
      result <- keep_types(x, i, invert = invert, ...)
    } else if ("re" %in% class(i)) {
      result <- keep_re(x, i, invert = invert, ...)
    } else if (is.logical(i)) {
      i <- i[!is.na(i)]
      result <- keep_bool(x, i, invert = invert, ...) 
    } else {
      stop("unsupported type of subset criterion")
    }
  }
  result
}

`[<-.freqlist` <- function(x, i, ..., value) {
  stop("subset assignment is not supported for 'freqlist' objects")
}



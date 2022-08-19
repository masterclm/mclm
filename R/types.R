# Create and coerce to class ===================================================
# REVIEW document class itself here?
#' Build a 'types' object
#' 
#' This function builds an object of the class [`types`].
#'
#' @inheritParams freqlist
#' @inherit freqlist details
#' @return An object of the class `types`, which is based on a character vector.
#'   It has additional attributes and methods such as:
#'   - base [`print()`][print.types()], [as_data_frame()], [sort()] and
#'   [base::summary()] (which returns the number of items and of unique items),
#'   - [tibble::as_tibble()],
#'   - the [n_types()] getter and the [explore()] method,
#'   - subsetting methods such as [keep_types()], [keep_pos()], etc. including `[]`
#'   subsetting (see [brackets]).
#'   
#'   An object of class `types` can be merged with another by means of [types_merge()],
#'   written to file with [write_types()] and read from file with [write_types()].
#' 
#' @seealso [as_types()]
#' @export
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' (tps <- types(toy_corpus, as_text = TRUE))
#' print(tps)
#' 
#' as.data.frame(tps)
#' as_tibble(tps)
#' 
#' sort(tps)
#' sort(tps, decreasing = TRUE)
types <- function(x,
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
                  ngram_sep = "_",
                  ngram_n_open = 0,
                  ngram_open = "[]",
                  as_text = FALSE) {
  as_types(freqlist(x,
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
                    ngram_sep = ngram_sep,
                    ngram_n_open = ngram_n_open,
                    ngram_open = ngram_open,
                    as_text = as_text),
           remove_duplicates = FALSE, # removing duplicates already done
           sort = FALSE)              # sorting already done 
}

#' Coerce object to a vector of types
#' 
#' This function coerces an object, such as a character vector, to an object of
#' class [`types`].
#'
#' @param x Object to coerce
#' @param remove_duplicates Logical. Should duplicates be removed from `x`
#'   prior to coercing to a vector of types.
#' @param sort Logical. Should `x` be
#'   alphabetically sorted prior to coercing to a vector of types;
#'   this argument is ignored if `remove_duplicates` is `TRUE`,
#'   because the result of removing duplicates is always sorted.
#' @param ... Additional arguments (not implemented)
#'
#' @inherit types return
#' @seealso [types()]
#' @export
#'
#' @examples
#' 
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' 
#' flist <- freqlist(toy_corpus, re_token_splitter = "\\W+", as_text = TRUE)
#' print(flist, n = 1000)
#' (sel_types <- as_types(c("happily", "lived", "once")))
#' keep_types(flist, sel_types)
#' tks <- tokenize(toy_corpus, re_token_splitter = "\\W+")
#' print(tks, n = 1000)
#' tks[3:12] # idx is relative to selection
#' head(tks) # idx is relative to selection
#' tail(tks) # idx is relative to selection
as_types <- function(x,
                     remove_duplicates = TRUE,
                     sort = TRUE,
                     ...) {
  result <- x
  if ("freqlist" %in% class(result)) {
    result <- type_names(result)
  }
  if (is.null(result)) {
    result <- vector(mode = "character", length = 0)
  } else if (!"character" %in% class(result)) {
    result <- as.character(result)
  }
  result <- result[!is.na(result)] # remove NAs
  if (remove_duplicates && (length(result) > 0)) {
    result <- names(table(result))
  }
  if (sort) {
    result <- stringr::str_sort(result)
  }
  class(result) <- c("types",
                     setdiff(class(result), c("types", "tokens")))
  result
}

# S3 methods from mclm =========================================================

#' @rdname n_types
#' @exportS3Method n_types types
#' @export
n_types.types <- function(x, ...) {
  if (! "types" %in% class(x)) {
    stop("argument 'x' must be of the class 'types'")
  }
  without_duplicates <- length(table(x))
  with_duplicates <- length(x)
  if (without_duplicates < with_duplicates) {
    warning("duplicates detected and counted double in 'types' object")
  }
  with_duplicates
}  

#' @rdname explore
#' @exportS3Method explore types
#' @export
explore.types <- function(x,
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
            cur_hits <- grep(cur_regex, x, perl = perl)
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

## Subsetting ------------------------------------------------------------------
# (Including `[` even though it's not from mclm)

#' @rdname keep_pos
#' @exportS3Method drop_pos types
#' @export
drop_pos.types <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.types(x, pos, invert = TRUE, ...)
}

#' @rdname keep_pos
#' @exportS3Method keep_pos types
#' @export
keep_pos.types <- function(x, pos, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"types" %in% class(x)) {
    stop("x must be of class 'types'")
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
  if (length(x) == 0) {
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
    # create result  
    result <- subset_types(x, mtch)
  }
  # return result
  result
}

#' @rdname keep_types
#' @exportS3Method drop_types types
#' @export
drop_types.types <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.types(x, types, invert = TRUE, ...)
}

#' @rdname keep_types
#' @exportS3Method keep_types types
#' @export
keep_types.types <- function(x, types, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"types" %in% class(x)) {
    stop("x must be of class 'types'")
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
  # build result
  if (length(x) == 0) {
    result <- x
  } else {
    mtch <- !is.na(match(x, types)) # we avoid x_ranks[types] and x[types]
    if (invert) {
      mtch <- !mtch
    }
    result <- subset_types(x, mtch)
  }
  # return result
  result
}

#' @rdname keep_re
#' @exportS3Method drop_re types
#' @export
drop_re.types <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.types(x, pattern, perl = perl, invert = TRUE, ...)
}

#' @rdname keep_re
#' @exportS3Method keep_re types
#' @export
keep_re.types <- function(x, pattern, perl = TRUE, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"types" %in% class(x)) {
    stop("x must be of class 'types'")
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
  # -- build result
  if (length(x) == 0) {
    result <- x
  } else {
    sel <- grep(pattern[1], x, perl = perl[1], invert = invert[1])
    # create result
    if (length(sel) == 0) {
      result <- as_types(character(0))
    } else {
      result <- subset_types(x, sel)
    }
  }
  # return result
  result
}

#' @rdname keep_bool
#' @exportS3Method drop_bool types
#' @export
drop_bool.types <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.types(x, !bool, ...)
}

#' @rdname keep_bool
#' @exportS3Method keep_bool types
#' @export
keep_bool.types <- function(x, bool, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"types" %in% class(x)) {
    stop("x must be of class 'types'")
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
  # create result
  if (length(x) == 0) {
    result <- x
  } else {  
    result <- subset_types(x, bool)
  }
  # return result
  result
}

#' @rdname brackets
#' @exportS3Method `[` types
#' @export
`[.types` <- function(x, i, invert = FALSE, ...) {
  if (!"types" %in% class(x)) {
    stop("subsetted object must be of class 'types'")
  }
  result <- x
  if (!missing(i) && !is.null(i)) {
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


#' @rdname brackets
#' @exportS3Method `[<-` types
#' @export
`[<-.types` <- function(x, i, invert = FALSE, value) {
  stop("subset assignment is not supported for 'types' objects")
}


# S3 methods from other packages ===============================================
# (Including not supported functions)

#' @rdname as_data_frame
#' @exportS3Method as.data.frame types
#' @export
as.data.frame.types <- function(x, ...) {
  class(x) <- "character"
  data.frame(type = x, ...)
}

#' @exportS3Method tibble::as_tibble types
#' @export 
as_tibble.types <- function(x, ...) {
  tibble(type = x, ...)
}

#' @exportS3Method sort types
#' @export
sort.types <- function(x, decreasing = FALSE, ...) {
  as_types(sort(as_character(x),
               decreasing = decreasing,
               na.last = NA,
               ...),
           remove_duplicates = FALSE,  # not requested
           sort = FALSE)               # already done
}

#' @rdname stubs
#' @exportS3Method plot types
#' @export
plot.types <- function(x, ...) {
  warning("'types' objects have no plotting function; doing nothing")
  invisible(NULL)
}

#' @rdname mclm_print
#' @exportS3Method print types
#' @export
print.types <- function(x,
                        n = 20, from = 1,
                        sort_order = c("none", "alpha"),
                        extra = NULL,
                        ...) {
  # testing and processing argument 'x'
  if (!"types" %in% class(x)) {
    stop("x must be of the class 'types'")
  }
  n_types <- length(x)
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
  # testing and processing argument 'sort_order'
  if (is.null(sort_order)  ||
      is.na(sort_order[1]) ||
      (!sort_order[1] %in% c("alpha", "none"))) {
    sort_order <- "none"
    warning("unknown sort_order value found; 'none' assumed")
  }  
  if (n > 0) {
    idx <- from:(from + n - 1)
    ord <- idx # applies when sort_order is 'none'
    if (sort_order[1] == "alpha") {
      ord <- order(x)[idx]
    }  
  }
  # testing argument 'extra'
  if (!is.null(extra) && !is.environment(extra)) {
    stop("incorrect use of the argument 'extra'")
  }  
  # printing 'x'
  cat(mclm_style_dim(paste0(
    "Type collection of length ",
    n_types,
    "\n")))
  if (n > 0) {
    types <- x[ord]
    format_idx <- format(c("", 
                           format(idx, scientify = FALSE, 
                                  justify = "right")), 
                         justify = "right")    
    # we don't use format() [problems with unicode !]
    # nor do we use stringi::stri_pad_left [hickups with greek and Set.locale]
    nchar_types <- nchar(types)
    if (!is.null(extra$type_regex)) {
      types <- show_matches(types, extra$type_regex)
    }    
    format_types <- mclm_pad_left(
      c("type", types),
      max(nchar("type"), nchar_types),
      nchar_x = c(nchar("type"), nchar_types))
    # -- print titles
    cat(format_idx[1], " ", sep = "")
    cat(format_types[1], "\n", sep = "")
    # -- print horizontal lines
    cat(paste0(rep_len(" ", nchar(format_idx[1])), collapse = ""),
        " ",
        paste0(rep_len("-", nchar(format_types[1])), collapse = ""),
        sep = "")
    cat("\n")
    # -- optionally print dots
    if (from > 1) cat(mclm_style_very_dim("...\n"))
    # -- print items  
    for (j in seq_along(idx)) {
      cat(mclm_style_very_dim(format_idx[j + 1]), " ", 
          format_types[j + 1], "\n", sep = "")
    }
    # -- optionally print dots
    if ((from + n - 1) < n_types) cat(mclm_style_very_dim("...\n"))
  }
  invisible(x)
}


## Summary ---------------------------------------------------------------------

#' @exportS3Method summary types
#' @export
summary.types <- function(object, ...) {
  if (! "types" %in% class(object)) {
    stop("argument 'object' must be of the class 'types'")
  }
  result <- list()
  result$n_items <- length(object)
  result$n_unique_items <- length(table(object))
  class(result) <- "summary.types"
  result
}

#' @exportS3Method print summary.types
#' @export
print.summary.types <- function(x, ...) {
  if (!"summary.types" %in% class(x)) {
    stop("argument 'x' must be of the class 'summary.types'")
  }
  cat("Type collection of length ",
      x$n_items,
      "\n",
      sep = "")
  if (x$n_unique_items < x$n_items) {
    cat("[duplicates present and counted double]\n")
  }
  invisible(x)
}

#' @rdname stubs
#' @exportS3Method plot summary.types
#' @export
plot.summary.types <- function(x, ...) {
  warning("'summary.types' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# Public functions applied to the class ========================================

#' Merge 'types' objects
#' 
#' These methods merge two or more objects of class [`types`].
#'   
#' @param x,y An object of class [`types`]. 
#' @param ... Either objects of the class [`types`] or lists containing such objects.
#' @param sort Logical. Should the results be sorted.
#'
#' @return An object of the class [`types`].
#' @name merge_types
#'
#' @examples
#' (tps1 <- as_types(c("a", "simple", "simple", "example")))
#' (tps2 <- as_types(c("with", "a", "few", "words")))
#' (tps3 <- as_types(c("just", "for", "testing")))
#' types_merge(tps1, tps2)       # always removes duplicates, but doesn't sort
#' sort(types_merge(tps1, tps2)) # same, but with sorting
#' types_merge_all(tps1, tps2, tps3)
#' types_merge_all(list(tps1, tps2, tps3))
NULL

#' @describeIn merge_types Merge two types
#' @export
types_merge <- function(x, y, sort = FALSE) {
  if ((!"types" %in% class(x)) || (!"types" %in% class(y))) {
    stop("both x and y must be of the class 'types'")
  }
  types_merge_two(x, y, sort = sort)
}  

#' @describeIn merge_types Merge multiple types
#' @export
types_merge_all <- function(..., sort = FALSE) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car --
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("types" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("types_merge_all", car)
    }
  }   
  # -- processing cdr --
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("types_merge_all", cdr)
  }
  # -- merge results if needed --
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- types_merge_two(result_car, result_cdr)
  }
  # -- sort if needed --
  if (sort) {
    result <- as_types(result,
                      remove_duplicates = FALSE,
                      sort = TRUE)
  }
  # -- result --
  result
}

#' Read a vector of types from a text file
#' 
#' This function read an object of the class [`types`] from a text file. By default,
#' the text file is assumed to contain one type on each line.
#'
#' @param file Name of the input file.
#' @param sep If not `is.na(sep)`, then `sep` must be a character vector
#'   of length one. In that case, `sep` is interpreted as a
#'   type separator in the input file. This separator the serves as an
#'   additional type separator, next to the end of each line.
#'   The end of a line always indicated a separator between types (in other
#'   words, types cannot cross lines).
#' @param file_encoding The file encoding used in the input file.
#' @param trim_types Logical. Should leading and trailing
#'   white space should be stripped from the types.
#' @inheritParams as_types
#' @param ... Additional arguments (not implemented).
#'
#' @return Object of class [`types`].
#' @family reading functions
#' @seealso [write_types()]
#' @export
#'
#' @examples
#' \dontrun{
#'   types <- as_types(c("first", "second", "third"))
#'   write_types(types, "file_with_types.txt")
#'   types_2 <- read_types("file_with_types.txt")
#'   }
read_types <- function(file,
                       sep = NA,
                       file_encoding = "UTF-8",
                       trim_types = FALSE,
                       remove_duplicates = FALSE,
                       sort = FALSE,
                       ...) {
  result <- readr::read_lines(file,
                              locale = readr::locale(encoding = file_encoding))
  if (!is.na(sep) && is.character(sep)  && (length(sep) > 0)) {
    result <- unlist(stringr::str_split(result, sep[1]))
  }
  if (trim_types) {
    result <- stringr::str_trim(result) 
  }
  # QUESTION: config files is not read?
  # to consider: not run following line if config file says
  #              txt_comment_char is "" and not "#"
  result <- gsub('#.*$', '', result, perl = TRUE) 
  result <- result[nchar(result) > 0]
  result <- restore_unicode(result) 
  result <- as_types(result, 
                     remove_duplicates = remove_duplicates,
                     sort = sort)
  result
}

#' Write a vector of types to a text file
#' 
#' This function writes an object of the class [`types`] to a text file. Each type
#' is written to a separate line. The file encoding that is used is
#' `"UTF-8"`.
#'
#' @param x Object of class [`types`].
#' @param file Name of the output file
#' @param ... Additional arguments (not implemented).
#'
#' @return Invisibly, `x`.
#' @family writing functions
#' @seealso [read_types()]
#' @export
#'
#' @inherit read_types examples
write_types <- function(x,
                        file,
                        ...) {
  # TODO reinstate make_config file and use with read_types
  # TODO add encoding options?
  if (! "types" %in% class(x)) {
    stop("argument 'x' must be of the class 'types'")
  }
  # hide any real '#' that is actually part of a type
  x <- gsub('#', '<U+0023>', x, perl = TRUE) 
  readr::write_lines(x, file)
  invisible(x)
}

# Private functions applied to the class =======================================

#' Merge two 'types' objects
#'
#' @param x,y An object of class [`types`]
#' @param sort Whether or not to sort the result.
#'
#' @return An object of class [`types`]
#' @noRd
types_merge_two <- function(x, y, sort = FALSE) {
  as_types(dplyr::union(x, y),
           remove_duplicates = FALSE, # done by union
           sort = sort)              
}

#' Subset types
#'
#' @param x Object of class [`types`].
#' @param sel Numeric vector with positions or logical vector.
#'
#' @return Filtered object of class [`types`]
#' @noRd
subset_types <- function(x, sel) {
  result <- as.character(x)[sel]
  class(result) <- c("types",
                     setdiff(class(x),
                             c("tokens", "types")))
  result
}


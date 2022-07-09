#' Build a `types` object
#' 
#' Build an object of the class \code{types}.
#'
#' @param x the object \code{x} either contains the list of filenames of the
#'   corpus files (if \code{as_text} is \code{TRUE}) or the actual text
#'   of the corpus (if \code{as_text} is \code{FALSE}).
#'   If (if \code{as_text} is \code{TRUE}) and the length of the vector \code{x}
#'   is higher than one, then each item in \code{x} is treated as a separate
#'   line (or a separate series of lines) in the corpus text. Withing each
#'   item of \code{x}, the character \code{"\\\\n"} is also treated as
#'   a line separator.
#' @param re_drop_line if \code{re_drop_line} is \code{NULL}, then this argument is ignored.
#'   Otherwise, \code{re_drop_line} is a character vector (assumed to 
#'   be of length 1) containing a regular expression. Lines in \code{x}
#'   that contain a match for \code{re_drop_line} are
#'   treated as not belonging to the corpus and are excluded from the results.
#' @param line_glue if \code{line_glue} is \code{NULL}, then this argument is ignored.
#'   Otherwise, all lines in a corpus file (or in \code{x}, if
#'   \code{as_text} is \code{TRUE}, are glued together in one
#'   character vector of length 1, with the string \code{line_glue}
#'   pasted in between consecutive lines.  The value of \code{line_glue}
#'   can also be equal to the empty string \code{""}.
#'   The 'line glue' operation is conducted immediately after the 'drop line' operation.
#' @param re_cut_area if \code{re_cut_area} is \code{NULL}, then this argument is ignored.
#'   Otherwise, all matches in a corpus file (or in \code{x},
#'   if \code{as_text} is \code{TRUE}, are 'cut out' of the text prior
#'   to the identification of the tokens in the text (and are therefore
#'   not taken into account when identifying the tokens).
#'   The 'cut area' operation is conducted immediately after the 'line glue' operation.
#' @param re_token_splitter the actual token identification is either based on
#'   \code{re_token_splitter}, a regular expression that identifies the
#'   areas between the tokens, or on \code{re_token_extractor}, a regular
#'   expressions that identifies the area that are the tokens. The first
#'   mechanism is the default mechanism: the argument \code{re_token_extractor}
#'   is only used if \code{re_token_splitter} is \code{NULL}.
#'   More specifically, \code{re_token_splitter} is a regular expression
#'   that identifies the locations where lines in the corpus files are split into
#'   tokens. The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_token_extractor a regular expression that identifies the locations of the
#'   actual tokens. This argument is only used if \code{re_token_splitter} is \code{NULL}.
#'   Whereas matches for \code{re_token_splitter} are identified as the areas
#'   between the tokens, matches for \code{re_token_extractor} are
#'   identified as the areas of the actual tokens. Currently the implementation of
#'   \code{re_token_extractor} is a lot less time-efficient than that of \code{re_token_splitter}.
#'   The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_drop_token a regular expression that identifies tokens that are to
#'   be excluded from the results. Any token that contains a match for
#'   \code{re_drop_token} is removed from the results. If \code{re_drop_token}
#'   is \code{NULL}, this argument is ignored. The 'drop token' operation
#'   is conducted immediately after the 'token identification' operation.
#' @param re_token_transf_in a regular expression that identifies areas in the
#'   tokens that are to be transformed. This argument works together with the argument
#'   \code{token_transf_out}.
#'   If both \code{re_token_transf_in} and \code{token_transf_out} differ
#'   from \code{NA}, then all matches, in the tokens, for the
#'   regular expression  \code{re_token_transf_in} are replaced with
#'   the replacement string \code{token_transf_out}.
#'   The 'token transformation' operation is conducted immediately after the
#'   'drop token' operation.
#' @param token_transf_out a 'replacement string'. This argument works together with
#'   \code{re_token_transf_in} and is ignored if \code{re_token_transf_in} is \code{NULL}.
#' @param token_to_lower a boolean value that determines whether or not tokens must be converted
#'   to lowercase before returning the result.
#'   The 'token to lower' operation is conducted immediately after the
#'   'token transformation' operation.
#' @param perl a boolean value that determines whether or not the PCRE regular expression
#'   flavor is being used in the arguments that contain regular expressions.
#' @param blocksize number that indicates how many corpus files are read to memory
#'   `at each individual step' during the steps in the procedure;
#'   normally the default value of \code{300} should not
#'   be changed, but when one works with exceptionally small corpus files,
#'   it may be worthwhile to use a higher number, and when one works with
#'   exceptionally large corpus files, ot may be worthwhile to use a lower number.
#' @param verbose if \code{verbose} is \code{TRUE}, messages are printed to the console to
#'   indicate progress.
#' @param show_dots if \code{verbose} is \code{TRUE}, dots are printed to the console to
#'   indicate progress.
#' @param dot_blocksize if \code{verbose} is \code{TRUE}, dots are printed to the console to
#'   indicate progress.
#' @param file_encoding file encoding that is assumed in the corpus files.
#' @param ngram_size for a regular frequency list, i.e. a frequency list based on
#'   individual tokens, the value of \code{ngram_size} should be
#'   \code{NULL}; for a frequency list of ngrams of tokens,
#'   \code{ngram_size} should be a single number indicating the size of
#'   the ngrams. E.g. \code{2} for bigrams, or \code{3} for trigrams, etc.
#' @param ngram_sep Length one character vector containing the string that is used to
#'   separate tokens in the representation of ngrams.
#' @param ngram_n_open If \code{ngram_size} is some number higher than \code{1},
#'   and moreover \code{ngram_n_open} is a number higher than \code{0}, then 
#'   \code{freqlist()} works with n-grams with `open slots' in them. These
#'   n-grams with 'open slots' are generalisations of fully lexically specific
#'   n-grams (with the generalisation being that one or more of the items
#'   in the n-gram are replaced by a notation that stands for any arbitrary token').
#'   For instance, if \code{ngram_size} is \code{4} and \code{ngram_n_open} is
#'   \code{1}, and if moreover the corpora contains a
#'   4-gram \code{"it is widely accepted"}, then \code{freqlist()} will work with
#'   all modifications of \code{"it is widely accepted"} in which one (since
#'   \code{ngram_n_open} is \code{1}) of the items in the n-gram is
#'   replaced by an open slot. The first and the last item in
#'   an n-gram are never turned into an open slot; only the items in between
#'   are candidates for being turned into open slots. Therefore, in the
#'   example, the output will work with the n-grams \code{"it [] widely accepted"} and
#'   \code{"it is [] accepted"}. 
#'   As a second example, if if \code{ngram_size} is \code{5} and \code{ngram_n_open}
#'   is \code{2}, and if moreover the corpora contain a
#'   5-gram \code{"it is widely accepted that"}, then \code{freqlist()} will work
#'   with \code{"it [] [] accepted that"}, \code{"it [] widely [] that"}, and
#'   \code{"it is [] [] that"}.
#' @param ngram_open String that is used to represent open slots in n-grams.
#' @param as_text boolean vector, assumed to be of length 1, which determines whether
#'   \code{x} is to be interpreted as a character vector containing the
#'   actual contents of the corpus
#'   (if \code{as_text} is \code{TRUE}) or as a character vector containing the
#'   names of the corpus files (if \code{as_text} is \code{FALSE}).
#'   If if \code{as_text} is \code{TRUE}, then the arguments \code{blocksize},
#'   \code{verbose}, \code{show_dots}, \code{dot_blocksize},
#'   and \code{file_encoding} are ignored. 
#'
#' @return An object of the class \code{types}.
#'   The items in the output are sorted by their frequency in the input.
#' @export
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentence. And it lived happily ever after."
#' types(toy_corpus, as_text = TRUE)
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

#' Coerce `types` object to Data Frame
#'
#' The S3 method \code{as.data.frame}, when applied to an object \code{x} of the
#'   class \code{types}, coerces an object of the class \code{types} to a data frame.
#'   
#' @param x An object of class \code{types}.
#' @param ... Other arguments.
#'
#' @exportS3Method as.data.frame types
#' @return A dataframe. In this dataframe, the types sit in a column named \code{type}.
#' @export
#' @examples 
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentence. And it lived happily ever after."
#' 
#' (typs <- types(toy_corpus, as_text = TRUE))
#' 
#' as.data.frame(typs)
as.data.frame.types <- function(x, ...) {
  class(x) <- "character"
  data.frame(type = x, ...)
}

#' Coerce 'types' object to tibble
#' 
#' The S3 method \code{as_tibble}, when applied to an object \code{x} of the
#'   class \code{types}, coerces an object of the class \code{types} to a tibble.
#'
#' @param x An object of the class \code{types}.
#' @param ... Other arguments
#'
#' @return A tibble. In this tibble, the types sit in a colum named \code{type}.
#' @exportS3Method tibble::as_tibble types
#' @export
#' 
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentence. And it lived happily ever after."
#' 
#' (typs <- types(toy_corpus, as_text = TRUE))
#' 
#' as_tibble(typs)
as_tibble.types <- function(x, ...) {
  tibble(type = x, ...)
}


#' Sort a collection of types
#' 
#' Sort an object of the class \code{types}.
#' 
#' At the moment, types collections are not allowed to contain \code{NA} values.
#'   Therefore, no function argument is available
#'   that specifies how \code{NA} values should be sorted.
#'
#' @param x An object of the class \code{types}.
#' @param decreasing Logical value that indicates whether the items should be
#'   sorted from small to large (when \code{decreasing} in \code{FALSE})
#'   or from large to small (when \code{decreasing} is \code{TRUE}).
#'   The default value is \code{FALSE}.
#' @param ... Other arguments
#'
#' @return An object of class \code{types}.
#' @exportS3Method sort types
#' @export
#' 
#' @examples
#' (tps <- as_types(c("the", "a", "some", "no")))
#' sort(tps)
#' sort(tps, decreasing = TRUE)
sort.types <- function(x, decreasing = FALSE, ...) {
  as_types(sort(as_character(x),
               decreasing = decreasing,
               na.last = NA,
               ...),
           remove_duplicates = FALSE,  # not requested
           sort = FALSE)               # already done
}

#' Give number of types in a 'types' object
#' 
#' When applied to an object of class \code{types}, it returns the number of types.
#'
#' @param x An object of the class \code{types}.
#' @param ... Additional arguments
#'
#' @return The number of types in \code{x}.
#' 
#' @exportS3Method n_types types
#' @export
#'
#' @examples
#' (tks <- tokenize("The old man and the sea."))
#' n_tokens(tks)
#' (tps <- types(tks))
#' n_types(tps)
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

#' Merge 'types' objects
#'   
#' @param x,y An object of class \code{types}. 
#' @param ... Either objects of the class \code{types} or lists containing such objects.
#' @param sort Boolean value that indicates whether the result should be sorted.
#'
#' @return An object of the class \code{types}.
#' @name types_merge
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

#' @describeIn types_merge Merge two types
#' @export
types_merge <- function(x, y, sort = FALSE) {
  if ((!"types" %in% class(x)) || (!"types" %in% class(y))) {
    stop("both x and y must be of the class 'types'")
  }
  types_merge_two(x, y, sort = sort)
}  

#' @describeIn types_merge Merge multiple types
#' @export
types_merge_all <- function(..., sort = FALSE) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car ----------------------------
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("types" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("types_merge_all", car)
    }
  }   
  # -- processing cdr ----------------------------
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("types_merge_all", cdr)
  }
  # -- merge results if needed -------------------
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- types_merge_two(result_car, result_cdr)
  }
  # -- sort if needed ------------------------------------
  if (sort) {
    result <- as_types(result,
                      remove_duplicates = FALSE,
                      sort = TRUE)
  }
  # -- result ------------------------------------
  result
}

#' Merge two 'types' objects
#'
#' @param x,y An object of class \code{types}
#' @param sort Whether or not to sort the result.
#'
#' @return An object of class \code{types}
#' @noRd
types_merge_two <- function(x, y, sort = FALSE) {
  as_types(dplyr::union(x, y),
           remove_duplicates = FALSE, # done by union
           sort = sort)              
}

#' Coerce object to a vector of types
#'
#' @param x Object to coerce
#' @param remove_duplicates Length one boolean vector that determines whether or not
#'   duplicates are removed from \code{x} prior to coercing to a vector of types.
#' @param sort Length one boolean vector that determines whether or not
#'   \code{x} is alphabetically sorted prior to coercing to a vector of types;
#'   this argument is ignored if \code{remove_duplicates} is \code{TRUE},
#'   because the result of removing duplicates is always sorted.
#' @param ... Additional arguments (not implemented)
#'
#' @return An object of class \code{types}.
#' @export
#'
#' @examples
#' 
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentence. And it lived happily ever after."
#' 
#' flist <- freqlist(toy_corpus, re_token_splitter = "\\\\W+", as_text = TRUE)
#' print(flist, n = 1000)
#' (sel_types <- as_types(c("happily", "lived", "once")))
#' keep_types(flist, sel_types)
#' tks <- tokenize(toy_corpus, re_token_splitter = "\\\\W+")
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

#' @describeIn stubs Plot types
#' @exportS3Method plot types
#' @export
plot.types <- function(x, ...) {
  warning("'types' objects have no plotting function; doing nothing")
  invisible(NULL)
}


#' Succinct Description of a 'types' Object
#'
#' Build and/or print an object of the class \code{summary.types}.
#' 
#' @param object An object of class \code{types}.
#' @param x An object of class \code{summary.types}.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{summary.types}.
#' @name summary_types
#'
#' @examples
#' 
#' tps <- as_types(c("the", "a", "it", "is", "was", "are", "be", "being"))
#' summary(tps) 
#' print(summary(tps))
#' (tps_sum <- summary(tps))
#' names(tps_sum)
#' tps_sum[["n_types"]]
#' tps_sum$n_types
NULL

#' @describeIn summary_types Create a \code{summary.types} object
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

#' @describeIn summary_types Print a \code{summary.types} object
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

#' @describeIn stubs Plot summary of types
#' @exportS3Method plot summary.types
#' @export
plot.summary.types <- function(x, ...) {
  warning("'summary.types' objects have no plotting function; doing nothing")
  invisible(NULL)
}

#' Interactively navigate through 'types' object
#' 
#' This method only works in an interactive R session to open
#'   'exploration mode', in which the user can navigate through the \code{types}
#'   object \code{x} by means of brief commands. In 'exploration mode' the user can
#'   ask of a list of available commands by keying in \code{?}, followed by ENTER.
#'   The user can quiet 'exploration mode' by keying in \code{q}, followed by ENTER.
#'
#' @param x An object of class \code{types}.
#' @param n Maximum number of items in the \code{types} object to be shown at once.
#' @param from First item in the types object that is shown at the beginning of
#'   the explroation session.
#' @param perl Boolean. Whether or not regular expressions used in the exploration
#'   session use the PERL flavor of regular expression.
#' @param use_clear Boolean value. If \code{use_clear} is \code{TRUE},
#'   and if moreover the feature is supported by the R environment,
#'   the console will be cleared in between all interactive steps in the exploration session.
#' @param ... Additional arguments (not implemented)
#'
#' @return Invisibly, \code{x}
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



#' Print a vector of 'types'
#' 
#' Print objects of the class \code{types}.
#'
#' @param x An object of class \code{types}.
#' @param n Maximum number of types to print.
#' @param from Position of the first item to print.
#' @param sort_order Order in which the items are to be printed. Possible value
#'   are \code{"alpha"} (meaning that the items are to be sorted alphabetically),
#'   and \code{"none"} (meaning that the items are not to be sorted).
#' @param extra Extra settings.
#' @param ... Additional printing arguments.
#'
#' @return Invisibly, \code{x}.
#' @exportS3Method print types
#' @export
#'
#' @examples
#' x <- as_types(c("first", "second", "third"))
#' print(x, n = 1000)
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

#' Subset a 'types' object
#' 
#' Methods to subset objects of class \code{types} by position, list of types,
#' regex match or via boolean statements.
#' 
#'
#' The S3 methods starting with \code{keep_} (\code{keep_re()}, \code{keep_pos()},
#' \code{keep_bool()}, and \code{keep_types()}), when applied to an object of the
#' class \code{'types'}, take as their first argument \code{x}
#' an object of the class \code{'types'}, and extract from it those
#' items that \emph{match} the selection criterion which is their second argument,
#' viz. \code{pattern} in \code{keep_re()}, \code{pos} in \code{keep_pos()},
#' \code{bool} in \code{keep_bool()}, and \code{types} in \code{keep_types()}.
#' In this documentation, these methods are collectively called the \code{keep}-methods.
#' 
#' The S3 methods starting with \code{drop_} (\code{drop_re()}, \code{drop_pos()},
#' \code{drop_bool()}, and \code{drop_types()}), collectively called the \code{drop}-methods,
#' behave identical to how the \code{keep}-methods work when the argument \code{invert}
#' (which by default is \code{FALSE}) is set to \code{TRUE}.
#' In that case,  the items that \emph{do not match} the selection criterion are selected.
#' 
#' Subset selection for \code{'types'} object with the notation \code{[]},
#' in which case argument \code{i} is the selection criterion, behaves
#' similarly to the \code{keep}-methods. For more details on the relation
#' between the \code{[]} notation and the \code{keep}-methods, 
#' see the description of the argument \code{i}.
#' When the notation \code{x[i, \dots]} is used, it is also possible to
#' use the \code{invert} argument (which then is one of the additional
#' arguments in \code{\dots}). This \code{invert} argument
#' then serves the same purpose as the \code{invert} argument in the \code{keep}-methods.
#' When the notation \code{x[i, \dots]} is used, and no \code{invert} argument
#' is given, then \code{invert} is taken to be \code{FALSE}.
#'
#' @param x Object of class \code{types}.
#' @param pattern Either an object of the class \code{'re'} (see \code{\link{re}})
#'   or a character vector of length one, which contains a regular expression.
#' @param pos A numeric vector, the numbers in which identify positions (= indices)
#'   of items in \code{x}. If the numbers are positive, then their values point
#'   to the items that are to be selected. If the numbers are negative,
#'   then their absolute values point to the items that are not to be selected.
#'   Positive and negative numbers must not be mixed.
#' @param bool A logical vector of the same length as \code{x}. If \code{bool} is not
#'   of the correct length, it is \emph{recycled}. Assuming \code{invert} is
#'   \code{FALSE}, those items are selected for which \code{bool} is \code{TRUE}.
#' @param types Either an object of the class \code{'types'}
#'   (see \code{\link{types}} and \code{\link{as_types}}) or a character vector.
#'   Assuming \code{invert} is \code{FALSE},
#'   those items are selected the name of which is included in \code{types}.
#' @param i Selection criterion when subsetting with \code{[]}; depending on its
#'  class, it behaves differently:
#'   \describe{
#'     \item{\code{re}}{It works like \code{keep_re}.}
#'     \item{numeric}{It works like \code{keep_pos}.}
#'     \item{logical}{It works like \code{keep_bool}.}
#'     \item{\code{types} or character}{It works like \code{keep_types}.}
#'   }
#' @param perl Boolean vector of length one, which indicates whether or not
#'   \code{pattern} is treated as a PCRE flavor regular expression.
#'   The \code{perl} argument is only used if \code{pattern} is a regular character vector.
#'   If \code{pattern} is an object of the class \code{'re'}, then the
#'   \code{perl} argument is ignored, and the relevant information in the
#'   \code{'re'} object \code{pattern}, viz. the value of \code{pattern$perl}, is
#'   used instead.
#' @param invert Boolean vector of length one, which indicates whether the matches
#'   or the non-matches should be selected.
#' @param ... Additional arguments.
#'
#' @return Object of class \code{types} with the selected elements only.
#' @name subset_types
#'
#' @examples
#' 
#' (tps <- as_types(letters[1:10]))
#' 
#' keep_re(tps, "[acegi]")
#' drop_re(tps, "[acegi]")
#' 
#' keep_pos(tps, c(1, 3, 5, 7, 9))
#' drop_pos(tps, c(1, 3, 5, 7, 9))
#' 
#' keep_bool(tps, c(TRUE, FALSE))
#' drop_bool(tps, c(TRUE, FALSE))
#' 
#' keep_types(tps, c("a", "c", "e", "g", "i"))
#' drop_types(tps,  c("a", "c", "e", "g", "i"))
#' 
#' tps[re("[acegi]")]
#' tps[c(1, 3, 5, 7, 9)]
#' tps[c(TRUE, FALSE)]
#' tps[c("a", "c", "e", "g", "i")]
#' 
#' tps[re("[acegi]"), invert = TRUE]
#' tps[c(1, 3, 5, 7, 9), invert = TRUE]
#' tps[c(TRUE, FALSE), invert = TRUE]
#' tps[c("a", "c", "e", "g", "i"), invert = TRUE]
NULL

#' @describeIn subset_types Drop items by position
#' @exportS3Method drop_pos types
#' @export
drop_pos.types <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.types(x, pos, invert = TRUE, ...)
}

#' @describeIn subset_types Select items by position
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

#' @describeIn subset_types Drop items by list of types
#' @exportS3Method drop_types types
#' @export
drop_types.types <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.types(x, types, invert = TRUE, ...)
}

#' @describeIn subset_types Keep items by list of types
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
    # -- old code, which followed different logic -------------------------
    # # prepare creation of result
    # if (invert) {
    #   mtch <- match(x, types)
    #   mtch <- is.na(mtch)
    # } else {
    #   mtch <- match(types, x) # we avoid x_ranks[types] and x[types]
    #   mtch <- mtch[!is.na(mtch)]
    # }
    # ---------------------------------------------------------------------
    mtch <- !is.na(match(x, types)) # we avoid x_ranks[types] and x[types]
    if (invert) {
      mtch <- !mtch
    }
    # --------------------------------------------------------------------
    result <- subset_types(x, mtch)
  }
  # return result
  result
}

#' @describeIn subset_types Drop items by regular expression
#' @exportS3Method drop_re types
#' @export
drop_re.types <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.types(x, pattern, perl = perl, invert = TRUE, ...)
}

#' @describeIn subset_types Keep items by regular expression
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

#' @describeIn subset_types Drop items based on boolean statement
#' @exportS3Method drop_bool types
#' @export
drop_bool.types <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.types(x, !bool, ...)
}

#' @describeIn subset_types Keep items based on boolean statement
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

#' Subset types
#'
#' @param x Object of class \code{types}.
#' @param sel Numeric vector with positions or boolean vector.
#'
#' @return Filtered object of class \code{types}
#' @noRd
subset_types <- function(x, sel) {
  result <- as.character(x)[sel]
  class(result) <- c("types",
                     setdiff(class(x),
                             c("tokens", "types")))
  result
}



#' Read a vector of types from a text file
#' 
#' Reads an object of the class \code{'types'} from a text file. By default,
#' the text file is assumed to contain one type on each line.
#'
#' @param file Name of the input file.
#' @param sep If not \code{is.ba(sep)}, then \code{sep} must be a character vector
#'   of length one. In that case, \code{sep} is interpreted as a
#'   type separator in the input file. This separator the serves as an
#'   additional type separator, next to the end of each line.
#'   The end of a line always indicated a separator between types (in other
#'   words, types cannot cross lines).
#' @param file_encoding The file encoding used in the input file.
#' @param trim_types Boolean value that indicates whether or not leading and trailing
#'   whitespace should be stripped from the types.
#' @param remove_duplicates Length one boolean vector that determines whether or not
#'   duplicates are removed from \code{x} prior to coercing to a vector of class \code{types}.
#' @param sort Length one boolean vector that determines whether or not
#'   \code{x} is alphabetically sorted prior to coercing to a vector of types;
#'   this argument is ignored if \code{remove_duplicates} is \code{TRUE}, because the
#'   result of removing duplicates is always sorted.
#' @param ... Additional arguments (not implemented).
#'
#' @return Object of class \code{types}.
#' @seealso \code{\link{write_types}}
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


# public function write_types()
#  - writes a 'types' object to a txt file
#  - by default also creates an associated config file
# ------------------------------------------------------
write_types <- function(x,
                        file,
                        make_config_file = TRUE,
                        ...) {
  if (! "types" %in% class(x)) {
    stop("argument 'x' must be of the class 'types'")
  }
  # hide any real '#' that is actually part of a type
  x <- gsub('#', '<U+0023>', x, perl = TRUE) 
  readr::write_lines(x, file)
  if (make_config_file) {
    config <- list(data_class = "types",
                   txt_header = "FALSE",
                   txt_quote = "",
                   txt_comment_char = "#")
    write_config(config, file)
  }
  invisible(x)
}

#' @describeIn subset_types Keep items based on different criteria
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


`[<-.types` <- function(x, i, invert = FALSE, value) {
  stop("subset assignment is not supported for 'types' objects")
}

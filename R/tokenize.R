# Create and coerce to class ===================================================

#' Create or coerce an object into class `tokens`
#' 
#' `tokenize()` splits a text into a sequence of tokens, using regular expressions
#' to identify them, and returns an object of the class [`tokens`].
#' 
#' If the output contains ngrams with open slots, then the order
#' of the items in the output is no longermeaningful. For instance, let's imagine
#' a case where `ngram_size` is `5` and `ngram_n_open` is `2`.
#' If the input contains an 5-fgram `"it_is_widely_accepted_that"`, then the output
#' will contain `"it_[]_[]_accepted_that"`, `"it_[]_widely_[]_that"` and
#' `"it_is_[]_[]_that"`. The relative order of these three items in the output
#' must be considered arbitrary.
#'
#' @param x Either a character vector or an object of class
#'   `TextDocument` that contains the text to be tokenized.
#' @param re_drop_line `NULL` or character vector. If `NULL`, it is ignored.
#'   Otherwise, a character vector (assumed to be of length 1)
#'   containing a regular expression. Lines in `x`
#'   that contain a match for `re_drop_line` are
#'   treated as not belonging to the corpus and are excluded from the results.
#' @param line_glue `NULL` or character vector. If `NULL`, it is ignored.
#'   Otherwise, all lines in a corpus file (or in `x`, if
#'   `as_text` is `TRUE`), are glued together in one
#'   character vector of length 1, with the string `line_glue`
#'   pasted in between consecutive lines.
#'   The value of `line_glue` can also be equal to the empty string `""`.
#'   The 'line glue' operation is conducted immediately after the 'drop line' operation.
#' @param re_cut_area `NULL` or character vector. If `NULL`, it is ignored.
#'   Otherwise, all matches in a corpus file (or in `x`,
#'   if `as_text` is `TRUE`), are 'cut out' of the text prior
#'   to the identification of the tokens in the text (and are therefore
#'   not taken into account when identifying the tokens).
#'   The 'cut area' operation is conducted immediately after the 'line glue' operation.
#' @param re_token_splitter Regular expression or `NULL`.
#'   Regular expression that identifies the locations where lines in the corpus
#'   files are split into tokens. (See Details.)
#'   
#'   The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_token_extractor Regular expression that identifies the locations of the
#'   actual tokens. This argument is only used if `re_token_splitter` is `NULL`.
#'   (See Details.)
#'   
#'   The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_drop_token Regular expression or `NULL`. If `NULL`, it is ignored.
#'   Otherwise, it identifies tokens that are to
#'   be excluded from the results. Any token that contains a match for
#'   `re_drop_token` is removed from the results.
#'   The 'drop token' operation is conducted immediately after the 'token identification' operation.
#' @param re_token_transf_in Regular expression that identifies areas in the
#'   tokens that are to be transformed. This argument works together with the argument
#'   `token_transf_out`.
#'   
#'   If both `re_token_transf_in` and `token_transf_out` differ
#'   from `NA`, then all matches, in the tokens, for the
#'   regular expression  `re_token_transf_in` are replaced with
#'   the replacement string `token_transf_out`.
#'   
#'   The 'token transformation' operation is conducted immediately after the
#'   'drop token' operation.
#' @param token_transf_out Replacement string. This argument works together with
#'   `re_token_transf_in` and is ignored if `re_token_transf_in`
#'   is `NULL` or `NA`.
#' @param token_to_lower Logical. Whether tokens must be converted
#'   to lowercase before returning the result.
#'   The 'token to lower' operation is conducted immediately after the
#'   'token transformation' operation.
#' @param perl Logical. Whether the PCRE regular expression
#'   flavor is being used in the arguments that contain regular expressions.
#' @param ngram_size Argument in support of ngrams/skipgrams (see also `max_skip`).
#'   
#'   If one wants to identify individual tokens, the value of `ngram_size`
#'   should be `NULL` or `1`. If one wants to retrieve
#'   token ngrams/skipgrams, `ngram_size` should be an integer indicating
#'   the size of the ngrams/skipgrams. E.g. `2` for bigrams, or `3` for
#'   trigrams, etc.
#' @param max_skip Argument in support of skipgrams. This argument is ignored if
#'   `ngram_size` is `NULL` or is `1`.
#'   
#'   If `ngram_size` is `2` or higher, and `max_skip`
#'   is `0`, then regular ngrams are being retrieved (albeit that they
#'   may contain open slots; see `ngram_n_open`).
#'   
#'   If `ngram_size` is `2` or higher, and `max_skip`
#'   is `1` or higher, then skipgrams are being retrieved (which in the
#'   current implementation cannot contain open slots; see `ngram_n_open`).
#'   
#'   For instance, if `ngram_size` is `3` and `max_skip` is
#'   `2`, then 2-skip trigrams are being retrieved.
#'   Or if `ngram_size` is `5` and `max_skip` is
#'   `3`, then 3-skip 5-grams are being retrieved.
#' @param ngram_sep Character vector of length 1 containing the string that is used to
#'   separate/link tokens in the representation of ngrams/skipgrams
#'   in the output of this function.
#' @param ngram_n_open If `ngram_size` is `2` or higher, and moreover
#'   `ngram_n_open` is a number higher than `0`, then
#'   ngrams with 'open slots' in them are retrieved. These
#'   ngrams with 'open slots' are generalisations of fully lexically specific
#'   ngrams (with the generalisation being that one or more of the items
#'   in the ngram are replaced by a notation that stands for 'any arbitrary token').
#'   
#'   For instance, if `ngram_size` is `4` and `ngram_n_open` is
#'   `1`, and if moreover the input contains a
#'   4-gram `"it_is_widely_accepted"`, then the output will contain
#'   all modifications of `"it_is_widely_accepted"` in which one (since
#'   `ngram_n_open` is `1`) of the items in this n-gram is
#'   replaced by an open slot. The first and the last item inside
#'   an ngram are never turned into an open slot; only the items in between
#'   are candidates for being turned into open slots. Therefore, in the
#'   example, the output will contain `"it_[]_widely_accepted"` and
#'   `"it_is_[]_accepted"`.
#'   
#'   As a second example, if `ngram_size` is `5` and
#'   `ngram_n_open` is `2`, and if moreover the input contains a
#'   5-gram `"it_is_widely_accepted_that"`, then the output will contain
#'   `"it_[]_[]_accepted_that"`, `"it_[]_widely_[]_that"`, and
#'   `"it_is_[]_[]_that"`. 
#' @param ngram_open Character string used to represent open slots in ngrams in the
#'   output of this function.
#'
#' @return An object of class [`tokens`], i.e. a sequence of tokens.
#'   It has a number of attributes and method such as:
#'   - base [`print()`][print.tokens()], [as.data.frame()], [summary()],
#'   [sort()] and [rev()],
#'   - [tibble::as_tibble()],
#'   - an interactive [explore()] method,
#'   - some getters, namely [n_tokens()] and [n_types()],
#'   - subsetting methods such as [keep_types()], [keep_pos()], etc. including `[]`
#'   subsetting (see [brackets]).
#'   
#'   Additional manipulation functions include the [trunc_at()] method to ??,
#'   [tokens_merge()] and [tokens_merge_all()] to combine token lists and an
#'   [as_character()] method to convert to a character vector.
#'   
#'   Objects of class `tokens` can be saved to file with [write_tokens()];
#'   these files can be read with [read_freqlist()].
#' @name tokens
#' @seealso [as_tokens()]
#' @export
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' 
#' tks <- tokenize(toy_corpus)
#' print(tks, n = 1000)
#' 
#' tks <- tokenize(toy_corpus, re_token_splitter = "\\W+")
#' print(tks, n = 1000)
#' 
#' tokenize(toy_corpus, ngram_size = 3)
#' 
#' tokenize(toy_corpus, ngram_size = 3, max_skip = 2)
#' 
#' tokenize(toy_corpus, ngram_size = 3, ngram_n_open = 1)
tokenize <- function(
    x, 
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
  if (is.null(x) || length(x) == 0) {
    tokens <- vector(mode = "character", length = 0)
  } else {
    if ("TextDocument" %in% class(x)) {
      x <- as.character(x)
    } else  if (!is.character(x)) {
      x <- as.character(x)
    }
    x <- x[complete.cases(x)]
    if (length(x) == 0) {
      tokens <- vector(mode = "character", length = 0)
    }
  }
  if (!is.null(x) && length(x) > 0) {
    # -- process ngram_size and ngram_sep --
    if (!is.null(ngram_size) && !is.na(ngram_size[[1]])) {
      if (!is.numeric(ngram_size)) {
        stop("ngram_size must be either NA or a numeric value")
      }
      if (is.null(ngram_sep) || !is.character(ngram_sep[[1]])) {
        stop("ngram_sep must be a length one character vector")
      }    
    }
    # (further) split into lines --
    x <- unlist(strsplit(x, split = "\n"))
    # drop lines if needed --
    if (!is.null(re_drop_line) && !is.na(re_drop_line[[1]])) {
      x <- x[grep(re_drop_line[[1]], x, perl = perl, invert = TRUE)]
    }
    # paste lines in long line if needed --
    if (!is.null(line_glue) && !is.na(line_glue[[1]])) {
      x <- paste(x, collapse = line_glue[[1]])
    }
    # drop uninterestion regions if needed --
    if (!is.null(re_cut_area) && !is.na(re_cut_area[[1]])) {
      x <- gsub(re_cut_area[[1]], "", x, perl = perl)
    }
    # identify tokens --
    if (!is.null(re_token_splitter) && !is.na(re_token_splitter[[1]])) {
      tokens <- unlist(strsplit(x, re_token_splitter[[1]], perl = perl))
    } else {
      m <- gregexpr(re_token_extractor[[1]], x, perl = perl)
      tokens <- unlist(regmatches(x, m))
    }
    # -- drop tokens if needed --
    if (!is.null(re_drop_token) && !is.na(re_drop_token[[1]])) {
      tokens <- tokens[grep(re_drop_token[[1]], tokens,
                            perl = perl, invert = TRUE)]
    }
    # transform tokens if needed --
    if (!is.null(re_token_transf_in) && !is.na(re_token_transf_in[[1]])) {
      tokens <- gsub(re_token_transf_in[[1]], token_transf_out[[1]],
                     tokens, perl = perl)
    }
    # tokens to lower if needed --
    if (token_to_lower) {
      tokens <- tolower(tokens)
    }
    # drop length zero tokens --
    tokens <- tokens[nchar(tokens) > 0]
    # -- handle ngram_size --
    if (!is.null(ngram_size) && !is.na(ngram_size[[1]])) {
      tokens <- build_ngrams(tokens,
                             ngram_size = ngram_size[[1]],
                             max_skip = max_skip,
                             sep = ngram_sep,
                             n_open = ngram_n_open,
                             open = ngram_open)
    }
  }
  
  as_tokens(tokens)
}

#' Coerce object to class `tokens`
#' 
#' This function coerces a character object or another object that can be coerced
#' to a character into an object of class [`tokens`].
#'
#' @param x Object to coerce.
#' @param ... Additional arguments (not implemented).
#'
#' @return An object of class [`tokens`].
#' @export
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' 
#' tks <- tokenize(toy_corpus)
#' print(tks, n = 1000)
#' 
#' tks[3:12]
#' print(as_tokens(tokens[3:12]), n = 1000)
#' as_tokens(tail(tokens))
as_tokens <- function(x, ...) {
  result <- x
  if (is.null(result)) {
    result <- character(0)
  }
  if (! "character" %in% class(result)) {
    result <- as.character(result)
  }
  result <- result[!is.na(result)] # silently drop NAs
  class(result) <- c("tokens",
                     setdiff(class(result),
                             c("tokens", "types")))
  result
}

# S3 methods from mclm =========================================================

#' @rdname n_tokens
#' @exportS3Method n_tokens tokens
#' @export
n_tokens.tokens <- function(x, ...) {
  if (! "tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'tokens'")
  }
  length(x)
}  

#' @rdname n_types
#' @exportS3Method n_types tokens
#' @export
n_types.tokens <- function(x, ...) {
  if (! "tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'tokens'")
  }
  length(table(x))
}  

#' @rdname as_character
#' @exportS3Method as_character tokens
#' @export
as_character.tokens <- function(x, ...) {
  if (!"tokens" %in% class(x)) {
    stop("x must be of the class 'tokens'")
  }
  result <- x
  class(result) <- "character"
  result
}

#' @rdname explore
#' @exportS3Method explore tokens
#' @export
explore.tokens <- function(x,
                           n = 20,
                           from = 1,
                           perl = TRUE,
                           use_clear = TRUE,
                           ...) {
  if (interactive()) {
    length_x <- n_tokens(x)                    # n items in x
    cur_command <- "i"                         # "idle" (no change of state)
    cur_com_verb <- substr(cur_command, 1, 1)  # actual command 
    cur_regex <- ".*"                          # last regex that was used
    print_extra <- settings()                  # printing settings
    cur_hits <- numeric(0)                     # ids of hits for last regex
    while (cur_com_verb != "q") {
      ## -- initialize printing settings --
      assign("token_regex", NULL, envir = print_extra)
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
            assign("token_regex", cur_regex, envir = print_extra)
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
      if (!is.null(print_extra$token_regex)) {
        cat(mclm_style_dim(paste0("search pattern: ", print_extra$token_regex, "\n")))
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

#' @rdname trunc_at
#' @exportS3Method trunc_at tokens
#' @export
trunc_at.tokens <- function(x, pattern, 
                            keep_this = FALSE, 
                            last_match = FALSE, 
                            from_end = FALSE,
                            ...) {
  # -- test and process argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("x must be of class 'tokens'")
  }
  # -- test and process argument 'pattern'
  if (missing(pattern) || is.null(pattern)) {
    stop("pattern must not be unspecified")
  }
  if (!"re" %in% class(pattern)) {
    stop("pattern must be of class 're'")
  }
  # -- build result
  if ((length(x) == 0) || 
      (length(as_character(pattern)) == 0)) {
    result <- x
  } else {
    if (from_end) { x <- rev(x) }
    matches <- grep(as_character(pattern), 
                    x, 
                    perl_flavor(pattern))
    if (length(matches) > 0) {
      pos <- matches[1]
      if (last_match) {pos <- matches[length(matches)]}
      if (!keep_this) {pos <- pos - 1}
      x <- keep_pos(x, pmin(1, pos):pos)
    }
    if (from_end) { x <- rev(x) }
    result <- x
  }
  # return result
  result
}


## Subsetting ------------------------------------------------------------------
# public S3 function drop_pos()
drop_pos.tokens <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.tokens(x, pos, invert = TRUE, ...)
}

# public S3 function keep_pos()
keep_pos.tokens <- function(x, pos, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("x must be of class 'tokens'")
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
    result <- subset_tokens(x, mtch)
  }
  # return result
  result
}

# public S3 function drop_types()
drop_types.tokens <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.tokens(x, types, invert = TRUE, ...)
}

# public S3 function keep_types()
keep_types.tokens <- function(x, types, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("x must be of class 'tokens'")
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
    # prepare creation of result
    mtch <- !is.na(match(x, types)) # we avoid x_ranks[types] and x[types]
    if (invert) {
      mtch <- !mtch
    }
    # create result  
    result <- subset_tokens(x, mtch)
  }
  # return result
  result
}

# public S3 function drop_re()
drop_re.tokens <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.tokens(x, pattern, perl = perl, invert = TRUE, ...)
}



# public S3 function keep_re()
keep_re.tokens <- function(x, pattern, perl = TRUE, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("x must be of class 'tokens'")
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
      result <- tokenize(vector(mode = "character", length = 0))
    } else {
      result <- subset_tokens(x, sel)
    }
  }
  # return result
  result
}

# public S3 function drop_bool()
drop_bool.tokens <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.tokens(x, !bool, ...)
}

# public S3 function select_bool()
keep_bool.tokens <- function(x, bool, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("x must be of class 'tokens'")
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
    result <- subset_tokens(x, bool)
  }
  # return result
  result
}


`[.tokens` <- function(x, i, invert = FALSE, ...) {
  if (!"tokens" %in% class(x)) {
    stop("subsetted object must be of class 'tokens'")
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

# subset assignment for 'tokens' objects
# cases that need special treatment are:
#        x[i] <- NULL
#        x[] <- value
#        NAs in i # needs more testing and decisions; for the moment NAs
#                 #   lead to an error being returned
`[<-.tokens` <- function(x, i, invert = FALSE, ..., value) {
  # -- test argument 'x'
  if (!"tokens" %in% class(x)) {
    stop("subsetted object must be of class 'tokens'")
  }
  # -- test and process argument 'invert'
  if (is.null(invert)) {
    stop("invert must not be NULL")    
  }
  if (!is.logical(invert) || is.na(invert[1])) {
    stop("invert must either be TRUE or FALSE")
  }
  if (length(invert) > 1) {
    warning("invert contains multiple items; only invert[1] is used")
  }
  # -- test and process argument 'value'
  if (is.null(value)) {
    stop("replacement must not be NULL")    
  }
  # -- test and process argument 'i'
  if (!missing(i)) {
    if (is.null(i)) stop("subsetting criterion must not be NULL")
    if (any(is.na(i))) { # for the time being NAs in i are not supported
      stop("subsetting criterion must not contain NAs")
    }
  }  
  # -- do assignment
  sel <- NULL
  if (missing(i)) {
    if (invert) {
      sel <- integer(0)
    } else {
      sel <- 1:n_tokens(x)
    }
  } else if (is.numeric(i) || is.integer(i)) {
    i <- i[!is.na(i)] # this currently is a redundant instruction
    if (length(i) > 0) {
      i <- trunc(i)
      any_pos <- any(i >= 1)
      any_neg <- any(i <= -1)
      if (any_pos && any_neg) {
        stop("subsetting indices must be either all positive or all negative")          
      }
      if (any_neg) {
        invert <- !invert[1]
        i <- abs(i)
      }
      sel <- i[i >= 1 & i <= length(x)]
      if (invert[1]) {
        sel <- setdiff(1:length(x), sel)
      }
    } 
  } else if ("types" %in% class(i) || "character" %in% class(i)) {
    i <- i[!is.na(i)]  # this currently is a redundant instruction
    sel <- !is.na(match(x, i)) # we avoid as.character(x)[i]
    if (invert[1]) {
      sel <- !sel
    }
  } else if ("re" %in% class(i)) {
    sel <- grep(as_character(i),
                as_character(x),
                perl = perl_flavor(i),
                invert = invert[1])
  } else if (is.logical(i)) {
    i <- i[!is.na(i)]  # this currently is a redundant instruction
    if (length(x) != length(i)) i <- rep_len(i, length(x))
    if (invert[1]) i <- !i
    sel <- i
  } else {
    stop("unsupported type of subset criterion")
  }
  if (!is.null(sel)) {
    if (is.numeric(sel)) {
      n_affected <- length(sel)
    } else { # logical sel assumed
      n_affected <- sum(sel)
    }
    if (n_affected > 0) {
      x <- subset_tokens_assign(x, sel, value)
    }
  }
  # return result
  x
}

# S3 methods from other packages ===============================================
as.data.frame.tokens <- function(x, ...) {
  class(x) <- "character"
  data.frame(token = x, ...)
}


as_tibble.tokens <- function(x, ...) {
  tibble(token = x, ...)
}

sort.tokens <- function(x, decreasing = FALSE, ...) {
  as_tokens(sort(as_character(x),
                 decreasing = decreasing,
                 na.last = NA,
                 ...))
}
as.character.tokens <- function(x, ...) {
  if (!"tokens" %in% class(x)) {
    stop("x must be of the class 'tokens'")
  }
  result <- x
  class(result) <- "character"
  result
}

plot.tokens <- function(x, ...) {
  warning("'tokens' objects have no plotting function; doing nothing")
  invisible(NULL)
}
print.tokens <- function(x,
                         n = 20, from = 1,
                         extra = NULL,
                         ...) {
  # testing and processing argument 'x'
  if (! "tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'tokens'")
  }
  n_tokens <- length(x)
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
  n <- max(0, min(n, n_tokens - from + 1))
  # testing argument 'extra'
  if (!is.null(extra) && !is.environment(extra)) {
    stop("incorrect use of the argument 'extra'")
  }  
  # printing 'x'
  cat(mclm_style_dim(paste0(
    "Token sequence of length ",
    n_tokens,
    "\n")))
  if (n > 0) {
    idx <- from:(from + n - 1)
    tokens <- x[idx]
    format_idx <- format(c("idx", 
                           format(idx,
                                  scientify = FALSE, 
                                  justify = "right")), 
                         justify = "right")
    # we don't use format() [problems with unicode !]
    # nor do we use stringi::stri_pad_left [hickups with greek and Set.locale]
    nchar_tokens <- nchar(tokens)
    if (!is.null(extra$token_regex)) {
      tokens <- show_matches(tokens, extra$token_regex)
    }    
    format_tokens <- mclm_pad_left(
      c("token", tokens),
      max(nchar("token"), nchar_tokens),
      nchar_x = c(nchar("token"), nchar_tokens))
    # -- print titles
    cat(format_idx[1], " ", format_tokens[1], sep = "")
    cat("\n")
    # -- print horizontal lines
    cat(paste0(rep_len("-", nchar(format_idx[1])), collapse = ""),
        " ",
        paste0(rep_len("-", nchar(format_tokens[1])), collapse = ""),
        sep = "")
    cat("\n")
    # -- optionally print dots
    if (from > 1) cat(mclm_style_very_dim("...\n"))
    # -- print items  
    for (j in seq_along(idx)) {
      cat(mclm_style_very_dim(format_idx[j + 1]), " ",
          format_tokens[j + 1], "\n", sep = "")
    }
    # -- optionally print dots
    if ((from + n - 1) < n_tokens) cat(mclm_style_very_dim("...\n"))
  }
  invisible(x)
}

# public S3 method rev() for 'tokens' objects
rev.tokens <- function(x) {
  if (! "tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'tokens'")
  }
  as_tokens(rev(as_character(x)))
}
## Summary ---------------------------------------------------------------------

# public S3 function summary()
summary.tokens <- function(object, ...) {
  if (! "tokens" %in% class(object)) {
    stop("argument 'object' must be of the class 'tokens'")
  }
  result <- list()
  result$n_tokens <- n_tokens(object)
  class(result) <- "summary.tokens"
  result
}

# public S3 function summary()
print.summary.tokens <- function(x, ...) {
  if (!"summary.tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'summary.tokens'")
  }
  cat("Token sequence of length ",
      x$n_tokens,
      "\n",
      sep = "")
  invisible(x)
}

# public S3 function plot()
plot.summary.tokens <- function(x, ...) {
  warning("'summary.tokens' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# Public functions applied to the class ========================================

# public function read_tokens()
#  - reads a 'tokens' object from a txt file
#  - assumes each line contains one token
read_tokens <- function(file,
                        file_encoding = "UTF-8",
                        ...) {
  lines <- readr::read_lines(
    file,
    locale = readr::locale(encoding = file_encoding))
  lines <- lines[nchar(lines) > 0]          # drop empty lines
  result <- as_tokens(lines)
  result
}

# public function write_tokens()
#  - writes a 'tokens' object to a txt file
#  - by default does not create an associated config file
write_tokens <- function(x,
                         file,
                         make_config_file = FALSE,
                         ...) {
  if (! "tokens" %in% class(x)) {
    stop("argument 'x' must be of the class 'tokens'")
  }
  readr::write_lines(x, file)
  if (make_config_file) {
    config <- list(data_class = "tokens",
                   txt_header = "FALSE",
                   txt_quote = "",
                   txt_comment_char = "")
    write_config(config, file)
  }
  invisible(x)
}

# public function tokens_merge()
# merge two types objects
tokens_merge <- function(x, y) {
  if ((!"tokens" %in% class(x)) || (!"tokens" %in% class(y))) {
    stop("both x and y must be of the class 'tokens'")
  }
  tokens_merge_two(x, y)
}  

# public function tokens_merge_all()
# merge two or more tokens objects
tokens_merge_all <- function(...) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car --
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("tokens" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("tokens_merge_all", car)
    }
  }   
  # -- processing cdr --
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("tokens_merge_all", cdr)
  }
  # -- merge results if needed --
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- tokens_merge_two(result_car, result_cdr)
  }
  # -- result --
  result
}
# Private functions applied to the class =======================================

# private subset selection function
# x is assumed to be a tokens object (not tested)
# sel can be:
#  - numeric vector with positions
#  - boolean vector
subset_tokens <- function(x, sel) {
  result <- as.character(x)[sel]
  class(result) <- c("tokens",
                     setdiff(class(x),
                             c("tokens", "types")))
  result
}

# private function tokens_merge_two()
# both x and y are assumed to be of class "tokens"
tokens_merge_two <- function(x, y) {
  as_tokens(c(x, y))
}

# private subset selection function
# x is assumed to be a tokens object (not tested)
# sel can be:
#  - numeric vector with positions (positive values)
#  - boolean vector
subset_tokens_assign <- function(x, sel, value) {
  if (!"character" %in% class(value)) {
    value <- as.character(value)
  }
  value[is.na(value)] <- ""
  old_class <- class(x)
  x <- as_character(x)
  if (is.numeric(sel)) {
    value <- rep_len(value, length(sel))
    x[sel] <- value
  } else if (is.logical(sel)) {
    n_true <- sum(sel)
    if (n_true > 0) {
      value <- rep_len(value, n_true)
      x[sel] <- value    
    }
  }
  class(x) <- c("tokens",
                setdiff(old_class,
                        c("tokens", "types")))
  x
}


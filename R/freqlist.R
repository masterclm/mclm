# Create and coerce to class ===================================================

# REVIEW document class itself here?
#' Build the frequency list of a corpus
#' 
#' This function builds the word frequency list from a corpus.
#' 
#' The actual token identification is either based on the `re_token_splitter`
#' argument, a regular expression that identifies the areas between the tokens,
#' or on `re_token_extractor`, a regular expression that identifies the area
#' that are the tokens.
#' The first mechanism is the default mechanism: the argument `re_token_extractor`
#' is only used if `re_token_splitter` is `NULL`.
#' Currently the implementation of
#' `re_token_extractor` is a lot less time-efficient than that of `re_token_splitter`.
#'
#' @param x Either a list of filenames of the corpus files
#'   (if `as_text` is `TRUE`) or the actual text of the corpus
#'   (if `as_text` is `FALSE`).
#'   
#'   If `as_text` is `TRUE` and the length of the vector `x`
#'   is higher than one, then each item in `x` is treated as a separate
#'   line (or a separate series of lines) in the corpus text. Within each
#'   item of `x`, the character `"\\n"` is also treated as
#'   a line separator.
#' @param blocksize Number that indicates how many corpus files are read to memory
#'   `at each individual step' during the steps in the procedure;
#'   normally the default value of `300` should not
#'   be changed, but when one works with exceptionally small corpus files,
#'   it may be worthwhile to use a higher number, and when one works with
#'   exceptionally large corpus files, it may be worthwhile to use a lower number.
#' @param verbose If`TRUE`, messages are printed to the console to
#'   indicate progress.
#' @param show_dots,dot_blocksize If `TRUE`, dots are printed to the console to
#'   indicate progress.
#' @param file_encoding File encoding that is assumed in the corpus files.
#' @param as_text Logical.
#'    Whether `x` is to be interpreted as a character vector containing the
#'   actual contents of the corpus (if `as_text` is `TRUE`)
#'   or as a character vector containing the names of the corpus files
#'   (if `as_text` is `FALSE`).
#'   If if `as_text` is `TRUE`, then the arguments
#'   `blocksize`, `verbose`, `show_dots`, `dot_blocksize`,
#'   and `file_encoding` are ignored.
#' @inheritParams tokens
#'
#' @return An object of class `freqlist`, which is based on the class `table`.
#'   It has additional attributes and methods such as:
#'   - base [`print()`][print.freqlist()], [as_data_frame()],
#'   [summary()] and [`sort`][sort.freqlist()],
#'   - [tibble::as_tibble()],
#'   - an interactive [explore()] method,
#'   - various getters, including [tot_n_tokens()], [n_types()], [n_tokens()],
#'   values that are also returned by [summary()], and more,
#'   - subsetting methods such as [keep_types()], [keep_pos()], etc. including `[]`
#'   subsetting (see [brackets]).
#'   
#'   Additional manipulation functions include [type_freqs()] to extract the frequencies
#'   of different items, [freqlist_merge()] to combine frequency lists, and
#'   [freqlist_diff()] to subtract a frequency list from another.
#'   
#'   Objects of class `freqlist` can be saved to file with [write_freqlist()];
#'   these files can be read with [read_freqlist()].
#' @export
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' 
#' (flist <- freqlist(toy_corpus, as_text = TRUE))
#' print(flist, n = 20)
#' as.data.frame(flist)
#' as_tibble(flist)
#' summary(flist) 
#' print(summary(flist))
#' 
#' t_splitter <- "(?xi) [:\\s.;,?!\"]+"
#' freqlist(toy_corpus,
#'          re_token_splitter = t_splitter,
#'          as_text = TRUE)
#'          
#' freqlist(toy_corpus,
#'          re_token_splitter = t_splitter,
#'          token_to_lower = FALSE,
#'          as_text = TRUE)
#' 
#' t_extractor <- "(?xi) ( [:;?!] | [.]+ | [\\w'-]+ )"
#' freqlist(toy_corpus,
#'         re_token_splitter = NA,
#'         re_token_extractor = t_extractor,
#'         as_text = TRUE)
#' 
#' freqlist(letters, ngram_size = 3, as_text = TRUE)
#' 
#' freqlist(letters, ngram_size = 2, ngram_sep = " ", as_text = TRUE)
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

#' Coerce table to a frequency list
#' 
#' This function coerces an object of class [`table`] to an object of class [`freqlist`].
#'
#' @param x Object of class `table` or named numeric vector that will be
#'   interpreted as such.
#' @param tot_n_tokens Number representing the total number of tokens in the
#'   corpus from which the frequency list is derived. When `tot_n_tokens`
#'   is `NULL`, this total number of tokens will be taken to be the sum
#'   of the frequencies in `x`.
#' @param sort_by_ranks Logical.
#'   If `TRUE`, the items in the frequency list are sorted by frequency
#'   rank. If `FALSE`, the items in the frequency list, depending on the
#'   input type, either are sorted alphabetically or are not sorted at all.
#'
#' @inherit freqlist return
#'   
#' @seealso [freqlist()]
#' @export
#' 
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' 
#' ## make frequency list in a roundabout way
#' tokens <- tokenize(toy_corpus)
#' flist <- as_freqlist(table(tokens))
#' flist
#' 
#' ## more direct procedure
#' freqlist(toy_corpus, as_text = TRUE)
#' 
#' ## build frequency list from scratch: example 1
#' flist <- as_freqlist(c("a" = 12, "toy" = 53, "example" = 20))
#' flist
#' 
#' ## build frequency list from scratch: example 2
#' flist <- as_freqlist(c("a" = 12, "toy" = 53, "example" = 20),
#'                      tot_n_tokens = 1300)
#' flist
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
  # --- sorting by ranks --
  # this must be done before we assign classes and attributes, because
  # result <- result[ord] loses such information
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
  # assign classes and attributes etc. --
  if (!"freqlist" %in% class(result)) {
    class(result) <- c("freqlist", "table")
  }
  if (is.null(attr(result, "tot_n_tokens"))) {
    attr(result, "tot_n_tokens") <- sum(result)
  }
  if (!is.null(tot_n_tokens) && !is.na(tot_n_tokens[1])) {
    tot_n_tokens(result) <- tot_n_tokens[1]
  }
  result
}

# S3 methods from mclm =========================================================

#' @rdname explore
#' @exportS3Method explore freqlist
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

## Setters and getters ---------------------------------------------------------

#' @rdname n_tokens
#' @exportS3Method n_tokens freqlist
n_tokens.freqlist <- function(x, ...) {
  sum(x)
}  

#' @rdname n_types
#' @exportS3Method n_types freqlist
n_types.freqlist <- function(x, ...) {
  length(x)
}

#' @rdname type_names
#' @exportS3Method type_names freqlist
type_names.freqlist <- function(x, ...) {
  names(x)
}

#' @rdname tot_n_tokens
#' @exportS3Method `tot_n_tokens<-` freqlist
`tot_n_tokens<-.freqlist` <- function(x, value) {
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

#' @rdname tot_n_tokens
#' @exportS3Method tot_n_tokens freqlist
tot_n_tokens.freqlist <- function(x) {
  attr(x, 'tot_n_tokens')
}

#' @rdname orig_ranks
#' @exportS3Method `orig_ranks<-` freqlist
`orig_ranks<-.freqlist` <- function(x, value) {
  if (!is.null(value)) stop("value must be NULL")
  attr(x, 'orig_ranks') <- value
  x
}

#' @rdname orig_ranks
#' @exportS3Method orig_ranks freqlist
orig_ranks.freqlist <- function(x, with_names = FALSE, ...) {
  result <- attr(x, 'orig_ranks')
  if (!is.null(result) && with_names) {
    names(result) <- type_names(x)
  }
  result
}

#' @rdname ranks
#' @exportS3Method ranks freqlist
ranks.freqlist <- function(x, with_names = FALSE, ...) {
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

## Subsetting ------------------------------------------------------------------

#' @rdname keep_types
#' @exportS3Method drop_types freqlist
drop_types.freqlist <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.freqlist(x, types, invert = TRUE, ...)
}

#' @rdname keep_types
#' @exportS3Method keep_types freqlist
keep_types.freqlist <- function(x, types, invert = FALSE, ...) {
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
    return(x)
  }
   
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
  result
}

#' @rdname keep_re
#' @exportS3Method drop_re freqlist
drop_re.freqlist <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.freqlist(x, pattern, perl = perl, invert = TRUE, ...)
}

#' @rdname keep_re
#' @exportS3Method keep_re freqlist
keep_re.freqlist <- function(x, pattern, perl = TRUE, invert = FALSE, ...) {
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
    return(x)
  }
  # prepare creation of result
  sel <- grep(pattern[1], type_names(x), perl = perl[1], invert = invert[1])
  if (length(sel) == 0) {
    result <- freqlist("", as_text = TRUE)
    attr(result, "tot_n_tokens") <- attr(x, "tot_n_tokens")
  } else {
    result <- subset_freqlist(x, sel)
  }
  
  # return result
  result
}

#' @rdname keep_bool
#' @exportS3Method drop_bool freqlist
drop_bool.freqlist <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.freqlist(x, !bool, ...)
}

#' @rdname keep_bool
#' @exportS3Method keep_bool freqlist
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

#' @rdname keep_pos
#' @exportS3Method drop_pos freqlist
drop_pos.freqlist <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.freqlist(x, pos, invert = TRUE, ...)
}

#' @rdname keep_pos
#' @exportS3Method keep_pos freqlist
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

#' @rdname brackets
#' @exportS3Method `[` freqlist
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

#' @rdname stubs
#' @exportS3Method `[<-` freqlist
`[<-.freqlist` <- function(x, i, ..., value) {
  stop("subset assignment is not supported for 'freqlist' objects")
}

# S3 methods from other packages ===============================================

#' Sort a frequency list
#' 
#' This method sorts an object of class [`freqlist`].
#' 
#' Because of the way ranks are calculated for ties (with lower ranks being
#' assigned to ties earlier in the list), sorting the list may affect the
#' ranks of ties.
#' More specifically, ranks among ties may differ depending on the criterion
#' that is used to sort the frequency list.
#'
#' @param x Object of class [`freqlist`].
#' @param decreasing Logical. If `TRUE` items are sorted from large
#'   to small; if `FALSE`, from small to large.
#'   
#'   Note, however, that ranking in frequency lists is such that lower ranks
#'   correspond to higher frequencies. Therefore, sorting by rank (either
#'   `"ranks"` or `"orig_ranks"`) with `decreasing` set
#'   to its default value `FALSE` results in the highest frequencies
#'   ending up at the beginning of the sorted list.
#' @param sort_crit Character string determining the sorting criterion.
#'   
#'   If `sort_crit` is `"ranks"`, then the items in the frequency list
#'   are sorted by their current frequency rank.
#'   
#'   If `sort_crit` is `"names"`, then the items in the frequency
#'   list are sorted alphabetically their name.
#'   
#'   If `sort_crit` is `"orig_ranks"`, then the items in the frequency
#'   list are sorted by their original ranks (if those are present),
#'   or by their current frequency ranks (if no original ranks are present).
#'   
#'   Finally, sorting with `sort_crit` set to `"freqs"` is identical
#'   to sorting by frequency ranks, but with the meaning of the argument
#'   `decreasing` being reversed.
#'   In other words, sorting by frequencies (`"freqs"`) with `decreasing` set
#'   to its default value `FALSE` results in the lowest frequencies
#'   ending up at the beginning of the sorted list.
#' @param na_last Logical defining the behavior of `NA` elements.
#'   
#'   This argument is only relevant when `sort_crit` is `"orig_ranks"`
#'    because currently names and frequencies are not allowed to be `NA`
#'    in frequency lists.
#'    
#'    If `na_last` is `TRUE`, then items with a sorting criterion of
#'    `NA` end up at the end of the sorted frequency list.
#'    If `na_last` is `FALSE`, then items with a sorting criterion
#'    of `NA` end up at the start of the sorted frequency list.
#'    If `na_last` is `NA`, then items with a sorting criterion of
#'    `NA` are removed from the sorted frequency list.
#' @param ... Additional arguments.
#'
#' @return Object of class [`freqlist`].
#' @exportS3Method sort freqlist
#'
#' @examples
#' (flist <- freqlist(tokenize("the old story of the old man and the sea.")))
#' sort(flist)
#' sort(flist, decreasing = TRUE)
sort.freqlist <- function(x,
                          decreasing = FALSE,
                          sort_crit = c("ranks", "names",
                                        "orig_ranks", "freqs"),
                          na_last = TRUE,
                          ...) {
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

#' @rdname as_data_frame
#' @exportS3Method as.data.frame freqlist
as.data.frame.freqlist <- function(x,
                                   row.names = NULL,
                                   optional = FALSE,
                                   ...) {
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

#' @exportS3Method tibble::as_tibble freqlist
as_tibble.freqlist <- function(x, ...) {
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

# IDEA a rank/frequency plot could be an option, right?
#' @rdname stubs
#' @exportS3Method plot freqlist
plot.freqlist <- function(x, ...) {
  warning("'freqlist' objects have no plotting function; doing nothing")
  invisible(NULL)
}

#' @rdname mclm_print
#' @exportS3Method print freqlist
print.freqlist <- function(x,
                           n = 20, from = 1,
                           extra = NULL,
                           ...) {
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

## Summary ---------------------------------------------------------------------

#' @exportS3Method summary freqlist
summary.freqlist <- function(object, ...) {
  result <- list(
    n_types = n_types(object),
    n_tokens = n_tokens(object),
    tot_n_tokens = tot_n_tokens(object)
  )
  class(result) <- "summary.freqlist"
  result
}

#' @exportS3Method print summary.freqlist
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

#' @rdname stubs
#' @exportS3Method plot summary.freqlist
plot.summary.freqlist <- function(x, ...) {
  warning("'summary.freqlist' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# Public functions applied to the class ========================================

#' Retrieve frequencies from 'freqlist' object
#' 
#' `type_freq` and `type_freqs` retrieve the frequency of all or
#' some of the items of a [`freqlist`] object.
#'
#' @param x Object of class [`freqlist`].
#' @param types `NULL` or a character vector or an object of the class
#'   [`types`].
#'   
#'   If the argument `types` is `NULL`, then the frequencies of all
#'   the items in `x` are returned, in the order in which
#'   these items appear in `x`.
#'   
#'   If the argument `types` is a character vector or an object of the
#'   class [`types`], then only the frequencies (in `x`)
#'   of the items in `types` are given,
#'   in the order in which these items appear in `types`.
#'   For all items in `types` that do not occur in `x`,
#'   a frequency of zero is returned.
#' @param with_names Logical. Whether or not the items in the output should
#'   be given names. If `with_names` is `TRUE`, then the names
#'   of the types in the frequency list are used as names.
#' @param ... Additional arguments.
#'
#' @return Numeric vector representing the frequencies of the items.
#' @export
#' @seealso type_names
#'
#' @examples
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' type_freqs(flist) # frequencies of all items
#' type_names(flist) # names of all items
#' 
#' type_freqs(flist, with_names = TRUE) # frequencies of all types, with names
#' type_freqs(flist, c("man", "the")) # frequencies of specific items ...
#' type_freqs(flist, c("the", "man")) # ... in the requested order
#' type_freq(flist, "the")            # frequency of one item
#' 
#' # frequencies of specific items can also be printed using subsetting
#' flist[c("the", "man")] 
#' flist["the"]
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

#' @rdname type_freqs
#' @export
type_freq <- function(x, types = NULL, with_names = FALSE, ...) {
  type_freqs(x, types = types, with_names = with_names, ...)
}

#' Merge frequency lists
#' 
#' These functions merge two or more frequency lists, adding up the frequencies.
#' In the current implementation, original ranks are lost when merging.
#'
#' @param x,y An object of class [`freqlist`].
#' @param ... Various objects of class [`freqlist`] or a list of
#'   objects of class [`freqlist`]. 
#'
#' @return An object of class [`freqlist`].
#' @name merge_freqlist
#' @export
#'
#' @examples
#' (flist1 <- freqlist("A first toy corpus.", as_text = TRUE))
#' (flist2 <- freqlist("A second toy corpus.", as_text = TRUE))
#' (flist3 <- freqlist("A third toy corpus.", as_text = TRUE))
#' 
#' freqlist_merge(flist1, flist2)
#' 
#' freqlist_merge_all(flist1, flist2, flist3)
#' freqlist_merge_all(list(flist1, flist2, flist3)) # same result
freqlist_merge <- function(x, y) {
  if ((!"freqlist" %in% class(x)) || (!"freqlist" %in% class(y))) {
    stop("both x and y must be of the class 'freqlist'")
  }
  as_freqlist(freqlist_merge_two(x, y)) # as_freqlist sorts types by rank
}  

#' @rdname merge_freqlist
#' @export
freqlist_merge_all <- function(...) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car --
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("freqlist" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("freqlist_merge_all", car)
    }
  }   
  # -- processing cdr --
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("freqlist_merge_all", cdr)
  }
  # -- merge results if needed --
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- freqlist_merge_two(result_car, result_cdr)
  }
  # -- result --
  as_freqlist(result) # as_freqlist sorts types by rank
}

#' Subtract frequency lists
#' 
#' This function merges information from two frequency lists, subtracting the frequencies found
#' in the second frequency lists from the frequencies found in the first list.
#'
#' @param x,y Objects of class [`freqlist`].
#'
#' @return An object of class [`freqlist`].
#' @export
#'
#' @examples
#' (flist1 <- freqlist("A first toy corpus.", as_text = TRUE))
#' (flist2 <- freqlist("A second toy corpus.", as_text = TRUE))
#' 
#' freqlist_diff(flist1, flist2)
freqlist_diff <- function(x, y) {
  if ((!"freqlist" %in% class(x)) || (!"freqlist" %in% class(y))) {
    stop("both x and y must be of the class 'freqlist'")
  }
  names <- dplyr::union(names(x), names(y))
  tot_n_tokens_x <- attr(x, "tot_n_tokens")
  tot_n_tokens_y <- attr(y, "tot_n_tokens")
  # we avoid x[names] (and y[names]) because it is painfully slow
  # so we use the far more efficient x[match(names, names(x))] etc.
  x_new <- as.numeric(x)[match(names, names(x))]
  x_new[is.na(x_new)] <- 0
  
  y_new <- as.numeric(y)[match(names, names(y))]
  y_new[is.na(y_new)] <- 0
  
  result <- pmax(x_new - y_new, 0)
  names(result) <- names
  class(result) <- class(x) <- c("freqlist", "table")
  # CHANGED way of computing new tot_n_tokens in case y is a subset
  attr(result, "tot_n_tokens") <- max(c(
    tot_n_tokens_x - tot_n_tokens_y,
    tot_n_tokens_x - n_tokens(y),
    0))
  attr(result, "orig_ranks") <- NULL  
  as_freqlist(result)
}

#' Read a frequency list from a csv file
#' 
#' This function reads an object of the class [`freqlist`] from a csv file. The csv
#' file is assumed to contain two columns, the first being the type and the
#' second being the frequency of that type. The file is also assumed to
#' have a header line with the names of both columns.
#' 
#' `read_freqlist` not only reads the file `file`,
#' but also checks whether a configuration file exists with a name that
#' is identical to `file`, except that it has the filename extension
#' `".yaml"`.
#' 
#' If such a file exists, then that configuration file
#' is taken to 'belong' to `file` and is also read and the frequency list attributes
#' `"tot_n_tokens"` and `"tot_n_types"` are retrieved from it.
#' 
#' If no such configuration file exists,
#' then the values for `"tot_n_tokens"` and `"tot_n_types"` are
#' calculated on the basis of the frequencies in the frequency list. 
#'
#' @param file Character vector of length 1. Path to the input file.
#' @param sep Character vector of length 1. Column separator.
#' @param file_encoding File encoding used in the input file.
#' @param ... Additional arguments (not implemented).
#'
#' @return Object of class [`freqlist`].
#' @export
#' @family reading functions
#' @seealso [write_freqlist()]
#'
#' @examples
#' \dontrun{
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#' It consisted of three sentences. And it lived happily ever after."
#' freqs <- freqlist(toy_corpus, as_text = TRUE)
#' 
#' print(freqs, n = 1000)
#' write_freqlist(freqs, "example_freqlist.csv")
#' freqs2 <- read_freqlist("example_freqlist.csv")
#' print(freqs2, n = 1000)
#' }
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

#' Write a frequency list to a csv file
#' 
#' This function writes an object of the class [`freqlist`] to a csv file. The
#' resulting csv file contains two columns, the first being the type and the
#' second being the frequency of that type. The file also contains
#' a header line with the names of both columns. 
#' 
#' `write_freqlist` not only writes to the file `file`,
#' but also creates a configuration file with a name that
#' is identical to `file`, except that it has the filename extension
#' `".yaml"`. The frequency list attributes `"tot_n_tokens"`
#' and `"tot_n_types"` are stored to that configuration file. 
#'
#' @param x Object of class [`freqlist`].
#' @param file Character vector of length 1. Path to the output file.
#' @param sep Character vector of length 1. Column separator.
#' @param make_config_file Logical. Whether or not a configuration file
#'   needs to be created. In most circumstances, this should be set to `TRUE`.
#' @param ... Additional arguments (not implemented).
#'
#' @return Invisibly, `x`.
#' @export
#' @family writing functions
#' @seealso [read_freqlist()]
#'
#' @inherit read_freqlist examples
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

# Private functions applied to the class =======================================

#' Build a 'freqlist' on the basis of texts
#' 
#' Called by [freqlist()] when `x` contains the actual textual data.
#'
#' @param x Corpus text.
#' @inheritParams tokens
#'
#' @return Object of class [`freqlist`].
#' @noRd
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
    freqlist <- table(
      tokenize(x,
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

#' Build a 'freqlist' on the basis of texts in x
#' 
#' Called by [freqlist()] when `x` contains filenames.
#'
#' @param x Filenames of the corpus files
#' @inheritParams tokens
#'
#' @return Object of class [`freqlist`].
#' @noRd
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
  while (i <= n_texts) {
    j = 0
    blocktokens <- vector(mode = "list", length = min(blocksize, (n_texts-i+1)))
    while ((j < blocksize) && ((i + j) <= n_texts)) {
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
      # ==
      show_dot(show_dots && (((i + j) %% dot_blocksize) == 0))
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
  cat_if_verbose("\n", verbose)
  as_freqlist(globfreqlist) # sets class and attributes if needed
  # and sorts by ranks 
}

#' Subset freqlist
#'
#' @param x Object of class [`freqlist`].
#' @param sel Numeric vector with positions or logical vector.
#'
#' @return Filtered object of class [`freqlist`]
#' @noRd
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

#' Merge two frequency lists
#' 
#' This private function does not sort the types in the result by rank,
#' so this is something the public functions [freqlist_merge()] and 
#' [freqlist_merge_all()] need to take care of !!!
#' 
#' In the current implementation, orig_ranks are lost when merging, because
#' they are no longer necessarily unique.
#'
#' @param x,y Object of class [`freqlist`] 
#'
#' @return Object of class [`freqlist`]
#' @noRd
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

#' Reverse order of 'freqlist'
#'
#' @param x Object of class [`freqlist`].
#'
#' @return Object of class [`freqlist`].
#' @noRd
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

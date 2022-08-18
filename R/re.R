# Constructor ==================================================================

#' Build a regular expression
#' 
#' Create an object of class `re` or coerce a character vector to an object of
#' class `re`.
#' 
#' This class exists because some functions in the `r packageName()` package
#' require their arguments to be marked as being regular expressions.
#' For example, [keep_re()] does not need its `pattern` argument to be a [`re`]
#' object, but if the user wants to subset items with [brackets] using
#' a regular expression, they must use a [`re`] object.
#'
#' @param x Character vector of length one. The value of this character vector 
#'   is assumed to be a well-formed regular expression. In the current implementation
#'   this is assumed, not checked.
#' @param perl Logical. If `TRUE`, `x` is assumed to use PCRE (i.e. Perl
#'   Compatible Regular Expressions) notation. If `FALSE`, `x` is assumed to use
#'   base R's default regular expression notation.
#'   Contrary to base R's regular expression functions, [re()] assumes that the
#'   PCRE regular expression flavor is used by default.
#' @param ... Additional arguments.
#'
#' @return An object of class `re`, which is a wrapper around a character vector
#'   flagging it as containing a regular expression. In essence it is a named
#'   list: the `x` item contains the `x` input and the `perl` item contains
#'   the value of the `perl` argument (`TRUE` by default).
#'   
#'   It has basic methods such as [print()], [summary()] and [as.character()].
#'   
#' @export
#' 
#' @seealso [perl_flavor()], [scan_re()], [cat_re()]
#'
#' @examples
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#'   It consisted of three sentences. And it lived happily ever after."
#' 
#' (tks <- tokenize(toy_corpus))
#' 
#' # In `keep_re()`, the use of `re()` is optional
#' keep_re(tks, re("^.{3,}"))
#' keep_re(tks, "^.{3,}")
#' 
#' # When using brackets notation, `re()` is necessary
#' tks[re("^.{3,}")]
#' tks["^.{3,}"]
#' 
#' # build and print a `re` object
#' re("^.{3,}")
#' as_re("^.{3,}")
#' as.re("^.{3,}")
#' print(re("^.{3,}"))
re <- function(x, perl = TRUE, ...) {
  # -- test x for errors
  if (!"character" %in% class(x)) {
    stop("x must be of the class 'character'")
  }
  if (is.na(x[1])) {
    stop("x[1] must not be NA")
  }
  # -- test perl for errors
  if (!is.logical(perl)) {
    stop("perl must be a logical vector")
  }
  if (is.na(perl[1])) {
    stop("perl[1] must not be NA")
  }  
  # -- test x for peculiarities we want to warn about
  if (length(x) > 1) {
    warning("x contains multiple items; only x[1] is used")
  }
  # -- test perl for peculiarities we want to warn about
  if (length(perl) > 1) {
    warning("perl contains multiple items; only perl[1] is used")
  }
  # -- test ... argument
  extra_args <- names(list(...))
  if ("invert" %in% extra_args) {
    warning("unsupported invert argument is ignored")
  }
  # construct 're' object
  result <- list(x = x[1], perl = perl[1])
  class(result) <- "re"
  # return result
  result
}

#' @rdname re
#' @export
as_re <- function(x, perl = TRUE, ...) {
  re(x, perl = perl, ...)
}

# not a big fan of the following notation, but supporting it anyway
# IDEA remove?
#' @rdname re
#' @export
as.re <- function(x, perl = TRUE, ...) {
  re(x, perl = perl, ...)
}

# Public functions applied to the class ========================================

#' Retrieve or set the flavour of a regular expression
#' 
#' These functions retrieve or set the `perl` property of an object of class [`re`].
#' 
#' The assignment function merely sets the `perl` property so that the `x`
#' attribute is read as an expression using the PCRE flavor of regular expression
#' (when `perl = TRUE`) or not (when `perl = FALSE`).
#' The regular expression itself is not modified: if `perl` is set to an
#' inappropriate value, the regular expression will no longer function properly in
#' any of the functions that support [`re`] objects.
#'
#' @param x Object of class [`re`].
#' @param value Logical.
#'
#' @return A logical vector of length 1.
#' @export
#'
#' @examples
#' (regex <- re("^.{3,}"))
#' perl_flavor(regex)
#' 
#' perl_flavor(regex) <- FALSE
#' perl_flavor(regex)
#' regex
#' 
#' perl_flavor(regex) <- TRUE
#' perl_flavor(regex)
#' regex
perl_flavor <- function(x) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$perl
}

#' @rdname perl_flavor
#' @export
`perl_flavor<-` <- function(x, value) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$perl <- value
  x
}

# S3 Methods from other packages ===============================================

#' @exportS3Method print re
#' @export
print.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  cat("Regular expression (perl = ", x$perl, ")\n", sep = "")
  cat("------------------\n")
  cat(x$x, "\n")
}

#' @rdname stubs
#' @exportS3Method plot re
#' @export
plot.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  warning("there is no plot method for 're' objects; doing nothing")
  invisible(NULL)
}

#' @exportS3Method as.character re
as.character.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$x
}

## Summary ---------------------------------------------------------------------

#' @exportS3Method summary re
#' @export
summary.re <- function(object, ...) {
  if (!"re" %in% class(object)) {
    stop("object must be an object of the class 're'")
  }
  result <- object
  class(result) <- c("summary.re", "re")
  result
}

#' @exportS3Method  print summary.re
#' @export
print.summary.re <- function(x, ...) {
  if (!"summary.re" %in% class(x)) {
    stop("x must be an object of the class 'summary.re'")
  }
  cat("Regular expression (perl = ", x$perl, ")\n", sep = "")
  cat("------------------\n")
  cat(x$x, "\n")
}

#' @rdname stubs
#' @exportS3Method plot summary.re
plot.summary.re <- function(x, ...) {
  if (!"summary.re" %in% class(x)) {
    stop("x must be an object of the class 'summary.re'")
  }
  warning("there is no plot method for 'summary.re' objects; doing nothing")
  invisible(NULL)
}

# S3 Methods from mclm =========================================================

#' @rdname as_character
#' @exportS3Method as_character re
as_character.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$x
}

# Public regex convenience functions ===========================================

#' Convenience functions in support of regular expressions
#'
#' These functions are essentially simple wrappers around base R functions such as
#' [regexpr()], [gregexpr()], [grepl()], [grep()], [sub()] and [gsub()].
#' The most important differences between the functions documented here and the
#' R base functions is the order of the arguments (`x` before `pattern`) and the
#' fact that the argument `perl` is set to `TRUE` by default.
#' 
#' For some of the arguments (e.g. `perl`, `fixed`) the reader is directed to
#' [base R's regex documentation][base::regex].
#'
#' @param x Character vector to be searched or modified.
#' @param pattern Regular expression specifying what is to be searched.
#' @param replacement Character vector of length one specifying the replacement
#'   string. It is to be taken literally, except that the notation `\\1`, `\\2`, etc.
#'   can be used to refer to groups in `pattern`.
#' @param ignore.case Logical. Should the search be case insensitive?
#' @param perl Logical. Whether the regular expressions use the PCRE flavor
#'   of regular expression. Unlike in base R functions, the default is `TRUE`.
#' @param fixed Logical. If `TRUE`, `pattern` is a string to be matched as is,
#'   i.e. wildcards and special characters are not interpreted.
#' @param useBytes Logical. If `TRUE` the matching is done byte-by-byte rather than
#'   character-by-character. See 'Details' in [`grep()`].
#' @param requested_group Numeric.
#'   If `NULL` or `0`, the output will contain matches for `pattern` as a whole.
#'   If another number `n` is provided, then the output will not contain matches
#'   for `pattern` but instead will only contain the matches for the `n`th capturing
#'   group in `pattern` (the first if `requested_group = 1`, the second if
#'   `requested_group = 2`...).
#' @param drop_NA Logical. If `FALSE`, the output always has the same length as
#'   the input `x` and items that do not contain a match for `pattern` yield `NA`.
#'   If `TRUE`, such `NA` values are removed and therefore the result might contain
#'   fewer items than `x`.
#' @param unlist Logical. If `FALSE`, the output always has the same length as
#'   the input `x`. More specifically, the result will be a list in which input
#'   items that do not contain a match for `pattern` yield an empty vector, whereas
#'   input items that do match will yield a vector of at least length one (depending
#'   on the number of matches). If `TRUE`, the output is a single vector the length
#'   of which may be shorter or longer than `x`.
#' @param ... Additional arguments.
#'
#' @return `re_retrieve_first()`, `re_retrieve_last()` and `re_retrieve_all()` return
#'   either a single vector of character data or a list containing such vectors.
#'   `re_replace_first()` and `re_replace_all()` return the same type of character
#'   vector as `x`.
#'   
#'   `re_has_matches()` returns a logical vector indicating whether a match was
#'   found in each of the elements in `x`; `re_which()` returns a numeric
#'   vector indicating the indices of the elements of `x` for which a match was
#'   found.
#' 
#' @name re_convenience
#' @examples
#' x <- tokenize("This is a sentence with a couple of words in it.")
#' pattern <- "[oe](.)(.)"
#' 
#' re_retrieve_first(x, pattern)
#' re_retrieve_first(x, pattern, drop_NA = TRUE)
#' re_retrieve_first(x, pattern, requested_group = 1)
#' re_retrieve_first(x, pattern, drop_NA = TRUE, requested_group = 1)
#' re_retrieve_first(x, pattern, requested_group = 2)
#' 
#' re_retrieve_last(x, pattern)
#' re_retrieve_last(x, pattern, drop_NA = TRUE)
#' re_retrieve_last(x, pattern, requested_group = 1)
#' re_retrieve_last(x, pattern, drop_NA = TRUE, requested_group = 1)
#' re_retrieve_last(x, pattern, requested_group = 2)
#' 
#' re_retrieve_all(x, pattern)
#' re_retrieve_all(x, pattern, unlist = FALSE)
#' re_retrieve_all(x, pattern, requested_group = 1)
#' re_retrieve_all(x, pattern, unlist = FALSE, requested_group = 1)
#' re_retrieve_all(x, pattern, requested_group = 2)
#' 
#' re_replace_first(x, "([oe].)", "{\\1}")
#' re_replace_all(x, "([oe].)", "{\\1}")
NULL

#' @describeIn re_convenience Retrieve from each item in `x` the first match
#'   of `pattern`.
#' @export
re_retrieve_first <- function(x, pattern, 
                              ignore.case = FALSE, perl = TRUE,
                              fixed = FALSE, useBytes = FALSE,
                              requested_group = NULL,
                              drop_NA = FALSE,
                              ...) {
  if (is.null(x)) {
    stop('argument x must not be NULL')
  }
  m <- lapply(x, gregexpr, pattern = pattern,  
              ignore.case = ignore.case, perl = perl,
              fixed = fixed, useBytes = useBytes)
  result <- re_retrieve(x, m, requested_group)
  result <- unlist(lapply(result, h_get_first))
  if (drop_NA) {
    result <- result[!is.na(result)]
  }
  result
}


#' @describeIn re_convenience Retrieve from each item in `x`
#'   the last match of `pattern`.
#' @export
re_retrieve_last <- function(x, pattern, 
                             ignore.case = FALSE, perl = TRUE,
                             fixed = FALSE, useBytes = FALSE, 
                             requested_group = NULL,
                             drop_NA = FALSE,
                             ...) {
  if (is.null(x)) {
    stop('argument x must not be NULL')
  }
  m <- lapply(x, gregexpr, pattern = pattern,  
              ignore.case = ignore.case, perl = perl,
              fixed = fixed, useBytes = useBytes)
  result <- re_retrieve(x, m, requested_group)
  result <- unlist(lapply(result, h_get_last))
  if (drop_NA) {
    result <- result[!is.na(result)]
  }
  result
}

#' @describeIn re_convenience Retrieve from each item in `x`
#'   all matches of `pattern`.
#' @export
re_retrieve_all <- function(x, pattern, 
                            ignore.case = FALSE, perl = TRUE,
                            fixed = FALSE, useBytes = FALSE,
                            requested_group = NULL,
                            unlist = TRUE,
                            ...) {
  if (is.null(x)) {
    stop('argument x must not be NULL')
  }
  m <- lapply(x, gregexpr, pattern = pattern,  
              ignore.case = ignore.case, perl = perl,
              fixed = fixed, useBytes = useBytes)
  result <- re_retrieve(x, m, requested_group)
  if (unlist) { result <- unlist(result) }
  result
}

#' @describeIn re_convenience Simple wrapper around [grepl()].
#' @export
re_has_matches <- function(x, pattern, 
                           ignore.case = FALSE, perl = TRUE,
                           fixed = FALSE, useBytes = FALSE, 
                           ...) {
  grepl(pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
}

#' @describeIn re_convenience Simple wrapper around [grep()].
#' @export
re_which <- function(x, pattern, 
                     ignore.case = FALSE, perl = TRUE,
                     fixed = FALSE, useBytes = FALSE, 
                     ...) {
  grep(pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
}

#' @describeIn re_convenience Simple wrapper around [sub()].
#' @export
re_replace_first <- function(x, pattern, replacement,
                             ignore.case = FALSE, perl = TRUE,
                             fixed = FALSE, useBytes = FALSE, 
                             ...) {
  sub(pattern, replacement, x, ignore.case = ignore.case, 
      perl = perl, fixed = fixed, useBytes = useBytes)
}

#' @describeIn re_convenience Simple wrapper around [gsub()].
#' @export
re_replace_all <- function(x, pattern, replacement,
                           ignore.case = FALSE, perl = TRUE,
                           fixed = FALSE, useBytes = FALSE, ...) {
  gsub(pattern, replacement, x, ignore.case = ignore.case, 
       perl = perl, fixed = fixed, useBytes = useBytes)
}

# Private helper functions =====================================================

#' Private function to retrieve matches
#' 
#' This function groups a sequence of steps common to [re_retrieve_first()],
#' [re_retrieve_last()] and [re_retrieve_all()].
#' 
#' @param x Character string to find matches in
#' @param m List, result from [gregexpr()]
#' @inheritParams re_convenience
#' 
#' @return list or vector of matches
#' @noRd
re_retrieve <- function(x, m, requested_group) {
  if (is.null(requested_group) || requested_group == 0) {
    result <- Map(function(x, m) h_regmatches(x, m)[[1]], x, m)
  } else if (length(m) > 0 &&
             requested_group >= 1 &&
             requested_group <= length(attr(m[[1]][[1]], 'capture.start')[1,])) {
    # NOTE in following line, changed m to m[[1]] on 2020-11-11
    # but switched back to m on 2021-07-29
    result <- Map(function(x, m) h_regmatches(x, m, group = requested_group)[[1]], x, m)
  } else {
    result <- rep(NA, length(x))
  }
  names(result) <- NULL
  result
}

# regcapturedmatches.R
# https://gist.github.com/GegznaV/57b4ff13e6d7a8a8344e
# https://gist.github.com/MrFlick/10413321

# NOTE if this is an internal function for which m will ALWAYS be a list the check is not necessary, right? (or it could be forced to be one?)
#' Helper functions to identify matches
#'
#' @param x A character vector with text to match
#' @param m A list, output from [gregexpr()], with the same length of `x`. Each
#'   item is a numeric vector with the indices of the matches in each item in `x`
#'   and at least three attributes:
#'   
#'   - `match.length`, a numeric vector with the length of the match in each case
#'     (-1 when no match is found);
#'   - `index.type`: "char" if the match positions and lengths count characters,
#'     "bytes" if they count bytes instead.
#'   - `useBytes`: logical. If `TRUE`, `index.type` is "bytes"; if `FALSE`,
#'     `index.type` is "char".
#'     
#'   If named capture is used, there are further attributes `"capture.start"`,
#'   `"capture.length"` and `"capture.names"`.
#' @param invert Logical. If `TRUE`, return what has **not** matched.
#' @param group Numeric. Label of group to retrieve.
#'
#' @return A list of captured groups (or everything but, if `invert = TRUE`).
#' @noRd
h_regmatches <- function(x, m, invert = FALSE, group = 0) {
  # CHANGED check for m as list removed because we always use output of gregexpr
  # CHANGED merged with h_regmatchesgroup because the difference was minimal
  
  if (length(x) != length(m)) # QUESTION necessary for a private function with limited usage?
    stop(gettextf("%s and %s must have the same length", 
                  sQuote("x"), sQuote("m")), domain = NA)
  
  if (any(unlist(lapply(m, attr, "useBytes")))) {# with this private usage, it will be the same for all elements
    # if we count bytes, we try to convert the Encoding to ASCII
    asc <- iconv(x, "latin1", "ASCII")
    # if the original text cannot be converted to ASCII (because it was not latin1)
    # that Encoding becomes "bytes"
    ind <- is.na(asc) | (asc != x)
    if (any(ind)) 
      Encoding(x[ind]) <- "bytes"
  }
  
  match_indices <- if (group == 0) {
    m
  } else {
    capture_starts <- lapply(m, attr, "capture.start")
    lapply(capture_starts, h_reg_group, group = group)
  }
  
  match_lengths <- if (group == 0) {
    lapply(m, attr, "match.length")
  } else {
    capture_lengths <- lapply(m, attr, "capture.length")
    lapply(capture_lengths, h_reg_group, group = group)
  }
    
  y <- if (invert) {
    Map(h_reg_invert, x, match_indices, match_lengths, USE.NAMES = FALSE)
  } else {
    Map(h_reg_retrieve, x, match_indices, match_lengths, USE.NAMES = FALSE)
  }
  names(y) <- names(x)
  y
}

# NOTE it made more sense to isolate h_reg_invert and h_reg_retrieve when the h_regmatches functions were separated
# but I think it's still good to have them separated so they can have their documentation...?
#' Obtain the (non) matches from a gregexpr output
#' 
#' For re_regmatches_ functions
#' 
#' @param u Searched character vector
#' @param so Starting indices of the matches (from gregexpr output)
#' @param ml Match length (From the attribute in gregexpr output)
#' 
#' @return Subsetted string with the match or, for `h_reg_invert()`, non-match
#' @noRd
h_reg_invert <- function(u, so, ml) {
  n <- length(so)
  # CHANGED it considered that so could be NA, but that won't happen with gregexpr output
  if (n == 1L) { # if there are no matches actually
    if (so == -1L) { # if there is no match
      return(u)
    }
  }
  
  beg <- if (n > 1L) { # if there are multiple matches
    eo <- so + ml - 1L # define end index of matches
    if (any(eo[-n] >= so[-1L])) {# avoid overlap
      stop(gettextf("need non-overlapping matches for %s", 
                    sQuote("invert = TRUE")), domain = NA)
    }
    c(1L, eo + 1L) # beginning of non matches
  } else {
    c(1L, so + ml) # everything but only match?
  }
  
  end <- c(so - 1L, nchar(u)) # before beginning of each match and end of string
  
  substring(u, beg, end)
}

#' @rdname h_reg_invert
#' @noRd
h_reg_retrieve <- function(u, so, ml) {
  if (length(so) == 1L) {
    if (is.na(so) || (so == -1L)) 
      return(character())
  }
  substring(u, so, so + ml - 1L)
}

#' Extract group data from gregexpr output
#'
#' For h_regmatches_ functions
#' 
#' @param capture_data "capture.start" or "capture.length" attributes from a gregexpr
#'   output.
#' @param group Number of the group to extract
#'
#' @return Vector with start or length values for the requested group
#' @noRd
h_reg_group <- function(capture_data, group) {
  if (ncol(capture_data) >= group) {
    capture_data[,group]
  } else {
    rep(NA, ncol(capture_data))
  }
}

#' Retrieve a specific match from a vector or list of matches
#'
#' @param x Vector or list of matches
#'
#' @return A specific match
#' @noRd
h_get_first <- function(x) {
  result <- NA
  if (!is.null(x) && length(x) > 0) {
    result <- x[[1]]
  }
  result
}

#' @rdname h_get_first
#' @noRd
h_get_last <- function(x) {
  result <- NA
  if (!is.null(x) && length(x) > 0) {
    result <- x[[length(x)]]
  }
  result
}

#' Remove spaces from string
#' 
#' @param x String
#' @param regex Regular expression, by default spaces at the beginning and end
#' @param perl Logical. Is PCRE flavor being used?
#' 
#' @return Stripped string
#' @noRd
h_strip <- function (x, regex = "^\\s+|\\s+$", perl = TRUE) {
  gsub(regex, "", x, perl = perl)
}


# TO TEST ======================================================================
# TODO everything below this point need thorough testing

scan_txt <- function() {
  result <- ""
  nlines <- 0
  while(grepl("\\S", newline <- readLines(n = 1), perl = TRUE)) {
    if (nchar(result) > 0) {
      result <- paste0(result, "\n")
    }
    result <- paste0(result, newline)
    nlines <- nlines + 1
  }
  if (nlines > 1) {
    result <- paste0(result, "\n")
  }
  result
}

scan_txt2 <- function() {
  x <- scan(what = "character", sep = "\n")
  paste(x, collapse = "\n")
}


scan_re <- function(perl = TRUE, ...) {
  x <- scan_txt()
  re(x, perl = perl, ...)
}

scan_re2 <- function(perl = TRUE, ...) {
  x <- scan_txt2()
  re(x, perl = perl, ...)
}

cat_re <- function(x,
                   format = c("plain", "R"),
                   as_single_line = TRUE) {
  if ("re" %in% class(x)) {
    xx <- x$x
  } else if (!is.character(x)) {
    xx <- as.character(x)
  } else {
    xx <- x
  }
  if (as_single_line) {
    xx <- gsub("(?<![[\\\\])#[^\n]*[\r\n]*", "", xx, perl = TRUE)
    xx <- gsub("[\r\n]+", " ", xx, perl = TRUE)
    xx <- gsub("\\s+", " ", xx, perl = TRUE)
    xx <- gsub("(^[ ]|[ ]$)", "", xx, perl = TRUE)
  }
  if (format[1] == "R") {
    xx <- gsub("\\\\", "\\\\\\\\", xx, perl = TRUE)
    xx <- gsub("\"", "\\\"", xx, perl = TRUE)
    xx <- paste0("\"", xx, "\"")
  }
  cat(xx)
  cat("\n\n")
  invisible(x)
}


# We'll document the generics and add the methods instead of documenting the
# methods individually (since it's harder for people to know how to search...)

#' Interactively navigate through an object
#' 
#' This method only works in an interactive R session to open
#' 'exploration mode', in which the user can navigate through the
#' object `x` by means of brief commands.
#' 
#' `explore()` is different from other R instructions because it does not
#' automatically stop executing and show a new regular prompt (`>`) in the console.
#' Instead it shows a special prompt (`>>`) at which you can use `explore()`-specific
#' commands. Note that at the special prompt `>>` none of the regular R instructions
#' will work. The instructions that do work at this prompt, for `explore()`, are
#' listed below. After each instruction the user must press `ENTER`.
#' 
#' - `b` (begin): The first items in `x` are shown.
#' - `e` (end): The last items in `x` are shown.
#' - `d` (down *n* items): The 'next page' of items is shown.
#' - `u` (up *n* items): The 'previous page' of items is shown.
#' - `n` (next item): The list/table shifts one item down the list.
#' - `p` (previous item): The list/table shifts one item up the list.
#' - `g {linenumber}` (go to...): Jump to line `{linenumber}`.
#'   
#'     E.g. `g 1000` will jump to the 1000th line.
#' - `f {regex}` (find...): Jump to the next item matching the regular expression `{regex}`.
#'   
#'     E.g. `f (?xi) astic $` will jump to the next item ending in `"astic"`.
#'     The software starts searching from the *second item* presently visible onwards.
#'  
#'     `f` will jump to the next item matching the last regular expression used with
#'     `f {regex}`.
#'   
#'      This command is **not** available when `x` is a [`conc`] object.
#' - `l` (left): In [`assoc_scores`] objects, move one column to the left.
#' - `r` (right): In [`assoc_scores`] objects, move one column to the right.
#' - `?`: A help page is displayed, showing all possible commands.
#' - `q` (quit): Terminate interactive session.
#' 
#' @param x An object of any of the classes for which the method is implemented.
#' @inheritParams mclm_print
#' @param perl Logical. Whether or not the regular expressions used in the
#'   exploration session use the PERL flavour of regular expression.
#' @param use_clear Logical. If `TRUE`, and if the feature is supported by the R
#'   environment, the console will be cleared in between all interactive steps
#'   in the exploration session.
#' @param ... Additional arguments.
#'
#' @return Invisibly, `x`.
#' @export
#' @order 1
explore <- function(x, ...) UseMethod("explore")

#' @rdname stubs
explore.default <- function(x, ...) invisible(x)

# Subsetters ===================================================================


#' Subset an object by index
#' 
#' These methods can be used to subset objects based on a numeric vector of indices.
#' 
#' The methods [keep_pos()] and [drop_pos()] are part of a family of methods of
#' the `r packageName()` package used to subset different objects. The methods
#' starting with `keep_` extract the items in `x` based on the criterion specified
#' by the second argument. In contrast, the methods starting with `drop_` *exclude*
#' the items that match the criterion in the same argument.
#' 
#' Calling a `drop_` method is equivalent to calling its `keep_` counterpart when
#' the `invert` argument is `TRUE`.
#' 
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#' @param invert Logical. Whether the matches should be selected rather than the
#'   non-matches.
#' @param pos A numeric vector, the numbers in which identify positions (= indices)
#'   of items in `x`.
#'   
#'   If the numbers are positive, then their values point
#'   to the items that are to be selected.
#'   
#'   If the numbers are negative,
#'   then their absolute values point to the items that are not to be selected.
#'   Positive and negative numbers must not be mixed.
#'
#' @return Object of the same class as `x` with the selected elements only.
#' @family subsetters
#' @order 1
#'
#' @examples
#' # For a 'freqlist' object --------------------
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' keep_pos(flist, c(2, 3))
#' 
#' # For a 'types' object -----------------------
#' (tps <- as_types(letters[1:10]))
#' 
#' keep_pos(tps, c(1, 3, 5, 7, 9))
#' drop_pos(tps, c(1, 3, 5, 7, 9))
#' 
#' # For a 'tokens' object ----------------------
#' (tks <- as_tokens(letters[1:10]))
#' 
#' keep_pos(tks, c(1, 3, 5, 7, 9))
#' drop_pos(tks, c(1, 3, 5, 7, 9))
keep_pos <- function(x,
                     pos,
                     invert = FALSE,
                     ...) UseMethod("keep_pos")

#' @noRd 
keep_pos.default <- function(x,
                             pos,
                             invert = FALSE,
                             ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' @rdname keep_pos
#' @exportMethod 
drop_pos <- function(x,
                     pos,
                     ...) UseMethod("drop_pos")

#' @noRd 
drop_pos.default <- function(x,
                             pos,
                             ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' Subset an object based on regular expressions
#' 
#' These methods can be used to subset objects based on a regular expression.
#' 
#' @inherit keep_pos details
#'
#' @inheritParams keep_pos
#' @param pattern Either an object of the class [`re`]
#'   or a character vector of length one containing a regular expression.
#' @param perl Logical.
#'   Whether `pattern` is treated as a PCRE flavour regular expression.
#'   The `perl` argument is only used if `pattern` is a regular character vector.
#'   If `pattern` is an object of the class [`re`], then the
#'   `perl` argument is ignored, and the relevant information in the
#'   [`re`] object `pattern`, viz. the value of `pattern$perl`, is
#'   used instead.
#'   
#' @return Object of the same class as `x` with the selected elements only.
#' @family subsetters
#' @order 1
#'
#' @examples
#' # For a 'freqlist' object --------------------
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' keep_re(flist, "[ao]")
#' drop_re(flist, "[ao]")
#' keep_re(flist, "[ao]", invert = TRUE) # same as drop_re()
#' 
#' # For a 'types' object -----------------------
#' (tps <- as_types(letters[1:10]))
#' 
#' keep_re(tps, "[acegi]")
#' drop_re(tps, "[acegi]")
#' 
#' # For a 'tokens' object ----------------------
#' (tks <- as_tokens(letters[1:10]))
#' 
#' keep_re(tks, "[acegi]")
#' drop_re(tks, "[acegi]")
keep_re <- function(x,
                    pattern,
                    perl = TRUE,
                    invert = FALSE,
                    ...) UseMethod("keep_re")

#' @noRd 
keep_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            invert = FALSE,
                            ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' @rdname keep_re
#' @order 2
#' @exportMethod 
drop_re <- function(x,
                    pattern,
                    perl = TRUE,
                    ...) UseMethod("drop_re")

#' @noRd 
drop_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' Subset an object based on a selection of types
#' 
#' These methods can be used to subset objects based on a list of types.
#' 
#' @inherit keep_pos details
#'
#' @inheritParams keep_pos
#' @param types Either an object of the class [`types`]
#'   or a character vector.
#'   
#' @return Object of the same class as `x` with the selected elements only.
#' @family subsetters
#' @order 1
#'
#' @examples
#' # For a 'freqlist' object ------------------------
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' keep_types(flist, c("man", "and"))
#' drop_types(flist, c("man", "and"))
#' keep_types(flist, c("man", "and"), invert = TRUE) # same as drop_types()
#' 
#' # For a 'types' object ---------------------------
#' (tps <- as_types(letters[1:10]))
#' 
#' keep_types(tps, c("a", "c", "e", "g", "i"))
#' drop_types(tps,  c("a", "c", "e", "g", "i"))
#' 
#' # For a 'tokens' object --------------------------
#' (tks <- as_tokens(letters[1:10]))
#' 
#' keep_types(tks, c("a", "c", "e", "g", "i"))
#' drop_types(tks,  c("a", "c", "e", "g", "i"))
keep_types <- function(x,
                       types,
                       invert = FALSE,
                       ...) UseMethod("keep_types")

#' @noRd 
keep_types.default <- function(x,
                               types,
                               invert = FALSE,
                               ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' @rdname keep_types
#' @order 2
#' @exportMethod 
drop_types <- function(x,
                       types,
                       ...) UseMethod("drop_types")

#' @noRd 
drop_types.default <- function(x,
                               types,
                               ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' Subset an object based on logical criteria
#' 
#' These methods can be used to subset objects based on a logical vector.
#' 
#' @inherit keep_pos details
#'
#' @inheritParams keep_pos
#' @param bool A logical vector of the same length as `x`. If `bool` is not
#'   of the correct length, it is *recycled*. Assuming `invert` is
#'   `FALSE`, those items are selected for which `bool` is `TRUE`.
#'   
#' @return Object of the same class as `x` with the selected elements only.
#' @family subsetters
#' @order 1
#'
#' @examples
#' # For a 'freqlist' object---------------------
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' keep_bool(flist, type_freqs(flist) < 2)
#' drop_bool(flist, type_freqs(flist) >= 2)
#' keep_bool(flist, ranks(flist) <= 3)
#' 
#' keep_bool(flist, c(FALSE, TRUE, TRUE, FALSE)) 
#' 
#' (flist2 <- keep_bool(flist, type_freqs(flist) < 2))
#' keep_bool(flist2, orig_ranks(flist2) > 2)
#' 
#' # For a 'types' object ----------------------
#' (tps <- as_types(letters[1:10]))
#' 
#' keep_bool(tps, c(TRUE, FALSE))
#' drop_bool(tps, c(TRUE, FALSE))
#' 
#' # For a 'tokens' object ----------------------
#' (tks <- as_tokens(letters[1:10]))
#' 
#' keep_bool(tks, c(TRUE, FALSE))
#' drop_bool(tks, c(TRUE, FALSE))
keep_bool <- function(x,
                      bool,
                      invert = FALSE,
                      ...) UseMethod("keep_bool")

#' @noRd 
keep_bool.default <- function(x,
                              bool,
                              invert = FALSE,
                              ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' @rdname keep_bool
#' @order 2
#' @exportMethod 
drop_bool <- function(x,
                      bool,
                      ...) UseMethod("drop_bool")

#' @noRd 
drop_bool.default <- function(x,
                              bool,
                              ...) {
  warning("unsupported type of x; simply returning x")
  x
}

#' Subset an object by different criteria
#' 
#' This method can be used to subset objects based on different criteria.
#' 
#' The subsetting method with the notation `[]`, applied to `r packageName()` objects,
#' is part of a family of subsetting methods: see [keep_pos()], [keep_re()],
#' [keep_types()] and [keep_bool()]. In this case, the argument `i` is the selection
#' criterion and, depending on its class, the method behaves different:
#' 
#' - providing a [`re`] object is equivalent to calling [keep_re()],
#' - providing a numeric vector is equivalent to calling [keep_pos()],
#' - providing a logical vector is equivalent to calling [keep_bool()],
#' - providing a [`types`] object or a character vector is equivalent to calling [keep_types()].
#' 
#' When the notation `x[i, ...]` is used, it is also possible to set the `invert`
#' argument to `TRUE` (which then is one of the additional arguments in `...`).
#' This `invert` argument then serves the same purpose as the `invert` argument
#' in the `keep_` methods, turning it into a `drop_` method.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#' @param invert Logical. Whether the matches should be selected rather than the
#'   non-matches.
#' @param i Selection criterion; depending on its class, it behaves differently.
#' @param value Value to assign.
#'
#' @return Object of the same class as `x` with the selected elements only.
#' @name brackets
#' @family subsetters
#' @order 1
#'
#' @examples
#' # For a 'freqlist' object --------------------
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' ## like keep_re()
#' flist[re("[ao]")]
#' flist[re("[ao]"), invert = TRUE]
#' 
#' ## like keep_pos()
#' flist[type_freqs(flist) < 2]
#' flist[ranks(flist) <= 3]
#' flist[ranks(flist) <= 3, invert = TRUE]
#' flist[2:3]
#' 
#' ## like keep_bool()
#' (flist2 <- keep_bool(flist, type_freqs(flist) < 2))
#' flist2[orig_ranks(flist2) > 2]
#' 
#' ## like keep_types()
#' flist[c("man", "and")]
#' flist[as_types(c("man", "and"))]
#' 
#' # For a 'types' object -----------------------
#' (tps <- as_types(letters[1:10]))
#' 
#' tps[c(1, 3, 5, 7, 9)]
#' tps[c(TRUE, FALSE)]
#' tps[c("a", "c", "e", "g", "i")]
#' 
#' tps[c(1, 3, 5, 7, 9), invert = TRUE]
#' tps[c(TRUE, FALSE), invert = TRUE]
#' tps[c("a", "c", "e", "g", "i"), invert = TRUE]
#' 
#' # For a 'tokens' object ----------------------
#' (tks <- as_tokens(letters[1:10]))
#' 
#' tks[re("[acegi]"), invert = TRUE]
#' tks[c(1, 3, 5, 7, 9), invert = TRUE]
#' tks[c(TRUE, FALSE), invert = TRUE]
#' tks[c("a", "c", "e", "g", "i"), invert = TRUE]
NULL

# Getters ======================================================================

#' Count tokens
#' 
#' This method returns the number of tokens in an object.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#'
#' @return A number.
#' @export
#' @family getters and setters
#' @order 1
#'
#' @examples
#' (tks <- tokenize("The old man and the sea."))
#' n_tokens(tks)
#' 
#' (flist <- freqlist(tks))
#' n_tokens(flist)
#' n_types(flist)
n_tokens <- function(x, ...) UseMethod("n_tokens")

#' @rdname stubs
n_tokens.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.numeric(NA)
}

#' Count types
#' 
#' This method returns the number of types in an object.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#'
#' @return A number.
#' @export
#' @family getters and setters
#' @order 1
#' 
#' @examples
#' (tks <- tokenize("The old man and the sea."))
#' 
#' # for a types object ----------
#' (tps <- types(tks))
#' n_types(tps)
#' 
#' # for a freqlist object -------
#' (flist <- freqlist(tks))
#' n_tokens(flist)
#' n_types(flist)
#' 
#' # for an assoc_scores object --
#' a <- c(10,    30,    15,    1)
#' b <- c(200, 1000,  5000,  300)
#' c <- c(100,   14,    16,    4)
#' d <- c(300, 5000, 10000, 6000)
#' types <- c("four", "fictitious", "toy", "examples")
#' 
#' (scores <- assoc_abcd(a, b, c, d, types = types))
#' n_types(scores)
n_types <- function(x, ...) UseMethod("n_types")

#' @rdname stubs
n_types.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.numeric(NA)
}

#' Return the names of the types in an object
#' 
#' This method returns the names of the types represented in an object.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#'
#' @return Character vector.
#' @family getters and setters
#' @order 1
#' @export
#'
#' @examples
#' # for a freqlist object
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' type_names(flist)
#' 
#' # for an assoc_scores object
#' a <- c(10,    30,    15,    1)
#' b <- c(200, 1000,  5000,  300)
#' c <- c(100,   14,    16,    4)
#' d <- c(300, 5000, 10000, 6000)
#' types <- c("four", "fictitious", "toy", "examples")
#' 
#' (scores <- assoc_abcd(a, b, c, d, types = types))
#' type_names(scores)
type_names <- function(x, ...) UseMethod("type_names")

#' @noRd
type_names.default <- function(x, ...) {
  warning("unsupported type of x; returning NA")
  as.character(NA)
}

#' Retrieve or set the total number of tokens
#' 
#' These methods retrieve or set the total number of tokens in
#' the corpus on which the frequency counts are based.
#' This total number of tokens may be higher than the sum of all frequency
#' counts in `x`, for instance, if `x` contains frequency counts
#' for a selection of items only, and not for all tokens in the corpus.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param value Numerical value.
#'
#' @return A number.
#' @family getters and setters
#' @order 1
#' 
#' @examples
#' x <- freqlist("The man and the mouse.",
#'               re_token_splitter = "(?xi) [:\\s.;,?!\"]+",
#'               as_text = TRUE)
#' x
#' tot_n_tokens(x)
#' 
#' y <- keep_types(x, c("man", "and"))
#' tot_n_tokens(y)
#' y
#' 
#' tot_n_tokens(y) <- sum(y)
#' y
#' tot_n_tokens(y)
tot_n_tokens <- function(x) UseMethod("tot_n_tokens")

#' @noRd
tot_n_tokens.default <- function(x) NULL

#' @rdname tot_n_tokens
#' @order 2
#' @exportMethod 
"tot_n_tokens<-" <- function(x, value) UseMethod("tot_n_tokens<-")

#' @noRd
"tot_n_tokens<-.default" <- function(x, value) x

#' Retrieve or set original ranks
#' 
#' These methods retrieve or set, for a the original ranks for the frequency
#' counts of an object.
#' These original ranks are only defined if `x` is the result of a selection
#' procedure (i.e. if `x` contains frequency counts for a selection of items
#' only, and not for all tokens in the corpus).
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param value Currently it can only be `NULL`.
#' @param with_names Logical. Whether or not the items in the output should
#'   be given names. If `TRUE`, then the names
#'   of the types in the frequency list are used as names.
#' @param ... Additional arguments.
#'
#' @return Either `NULL` or a numeric vector, representing the
#'   original ranks, with as its names the types to which these ranks apply.
#' @family getters and setters
#' @order 1
#'
#' @examples
#' x <- freqlist("The man and the mouse.",
#'               as_text = TRUE)
#' x
#' orig_ranks(x)
#' orig_ranks(x, with_names = TRUE)
#' 
#' y <- keep_types(x, c("man", "and"))
#' orig_ranks(y)
#' y
#' 
#' orig_ranks(y) <- NULL
#' y
#' orig_ranks(y)
#' 
#' tot_n_tokens(y) <- sum(y)
#' y
orig_ranks <- function(x, ...) UseMethod("orig_ranks")

#' @noRd
orig_ranks.default <- function(x, ...) NULL

#' @rdname orig_ranks
#' @order 2
#' @exportMethod 
"orig_ranks<-" <- function(x, value) UseMethod("orig_ranks<-")

#' @rdname orig_ranks
#' @exportMethod 
"orig_ranks<-.default" <- function(x, value) x

#' Retrieve the current ranks for frequency counts.
#' 
#' `ranks` retrieves from the ranks of its items in an object.
#' These ranks are integer values running from one up to the number of items
#' in `x`. Each items receives a unique rank.
#' Items are first ranked by frequency in descending order. Items with
#' identical frequency are further ranked by alphabetic order.
#' 
#' The `r packageName()` method [ranks()] is not
#' to be confused with [base::rank()]. There are two
#' important differences.
#' 
#' First,[base::rank()] always ranks items from low values to
#' high values and [ranks()] ranks from high
#' frequency items to low frequency items.
#' 
#' Second, [base::rank()] allows the user to choose among
#' a number of different ways to handle ties.
#' In contrast, [ranks()] always handles ties
#' in the same way. More specifically, items with identical frequencies
#' are always ranked in alphabetical order.
#' 
#' In other words, [base::rank()] is a flexible tool that
#' supports a number of different ranking methods that are commonly used in
#' statistics. In contrast, [ranks()] is a
#' rigid tool that supports only one type of ranking, which is a type of
#' ranking that is atypical from a statistics point of view, but is commonly
#' used in linguistic frequency lists. Also, it is designed to be unaffected
#' by the order of the items in the frequency list.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param with_names Logical. Whether or not the items in the output should
#'   be given names. If `TRUE`, then the names
#'   of the types in the frequency list are used as names.
#' @param ... Additional arguments.
#'
#' @return Numeric vector representing the current ranks, with as its names
#'   the types to which the ranks apply.
#' @family getters and setters
#' @order 1
#' @export
#'
#' @examples
#' (flist <- freqlist("The man and the mouse.", as_text = TRUE))
#' 
#' orig_ranks(flist)
#' ranks(flist)
#' ranks(flist, with_names = TRUE)
#' 
#' (flist2 <- keep_types(flist, c("man", "and")))
#' 
#' orig_ranks(flist2)
#' ranks(flist2)
ranks <- function(x, ...) UseMethod("ranks")

#' @noRd
ranks.default <- function(x, ...) NULL

#' Truncate a sequence of character data
#' 
#' This method takes as its argument `x` an object that represents a sequence of
#' character data, such as an object of class [`tokens`], and truncates it at the
#' position where a match for the argument `pattern` is found. Currently it is
#' only implemented for [`tokens`] objects.
#' 
#' @param x An object that represents a sequence of character data.
#' @param pattern A regular expression.
#' @param keep_this Logical. Whether the matching token itself should be kept.
#'   If `TRUE`, the truncating happens right after the matching token; if `FALSE`,
#'   right before.
#' @param last_match Logical. In case there are several matching tokens, if
#'   `last_match` is `TRUE`, the last match will be used as truncating point;
#'   otherwise, the first match will.
#' @param from_end Logical. If `FALSE`, the match starts from the first token progressing
#'   forward; if `TRUE`, it starts from the last token progressing backward.
#'   
#'   If `from_end` is `FALSE`, the part of `x` that is kept after truncation is
#'   the head of `x`. If it is `TRUE` instead, the part that is kept after truncation
#'   is the tail of `x`.
#' @param ... Additional arguments.
#' 
#' @return A truncated version of `x`.
#' @exportMethod 
#' 
#' @examples
#' (toks <- tokenize('This is a first sentence . This is a second sentence .',
#' re_token_splitter = '\\s+'))
#' 
#' trunc_at(toks, re("[.]"))
#' 
#' trunc_at(toks, re("[.]"), last_match = TRUE)
#' 
#' trunc_at(toks, re("[.]"), last_match = TRUE, from_end = TRUE)
trunc_at <- function(x, pattern, ...) UseMethod("trunc_at")
 
#' @noRd
trunc_at.default <- function(x, pattern, ...) invisible(x)

#' Coerce object to a numeric vector
#' 
#' This generic method turns its first argument `x` or at least part of the information
#' in it into a numeric object. It is an alternative notation for [base::as.numeric()].
#'
#' @param x An object to coerce.
#' @param ... Additional arguments.
#'   
#' @return A numeric vector.
#' @exportMethod 
#' @examples 
#' (flist <- freqlist(tokenize("The old story of the old man and the sea.")))
#' 
#' # extract frequency counts from a frequency list
#' as_numeric(flist)
#' as.numeric(flist)
#' 
#' # preferable alternative
#' type_freqs(flist)
as_numeric <- function(x, ...) UseMethod("as_numeric")

#' @rdname as_numeric
as_numeric.default <- function(x, ...) as.numeric(x, ...)

# From base ====================================================================

#' Print an object
#' 
#' This base method prints objects; here the arguments specific to `r packageName()`
#' implementations are described.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param n Maximum number of items in the object to be printed at once.
#' @param from Index of the first item to be printed.
#' @param from_col Index of the first column to be displayed in the regular area
#'   (among all selected columns, including frozen columns). If `from_col` points
#     to another column than the first one, then anything before that
#     column is not displayed. 
#' @param sort_order Order in which the items are to be printed. In general, possible values
#'   are `"alpha"` (meaning that the items are to be sorted alphabetically),
#'   and `"none"` (meaning that the items are not to be sorted).
#'   If `x` is an object of class [`assoc_scores`], a column name
#'   or vector of column names may be provided instead.
#' @param extra Extra settings, as an [environment]. Arguments defined here
#'   take precedence over other arguments. For instance, if `extra$from_col` is
#'   not `NULL`, it will overrule the `from_col` argument.
#' @param ... Additional printing arguments.
#' @param freeze_cols Names of columns that should not be affected by the argument
#'   `from_col`. Frozen columns are always printed to the left of non-frozen
#'   columns, even if in their original order was different. The names of the types
#'   are always and unavoidably printed as the leftmost column.
#'   
#'   If this argument is `NULL`, then the default setting applies, meaning that
#'   the following columns, if present, are displayed in the "frozen area": `a`,
#'   `PMI` and `G_signed`.
#'   
#'   To avoid any columns for being frozen, `freeze_cols` should be `NA` or
#'   `character(0)`.
#' @param keep_cols,drop_cols A vector of column names or `NULL`. If both arguments
#'   are `NULL`, all columns are printed (or as many as fit on the screen). If
#'   `keep_cols` is not `NULL`, it indicates the columns that should be printed.
#'   If it is `NULL` but `drop_cols` is not, then `drop_cols` indicates the columns
#'   that should *not* be printed. Note that they have **no effect** on the frozen area.
#'   
#'   Columns that are blocked from printing by these arguments are still available
#'   to `sort_order`.
#'
#' @return Invisibly, `x`.
#'   For objects of class `assoc_scores`, the output consists of two areas:
#'   the 'frozen area' on the left and the 'regular area' on the right. Both
#'   areas are visually separated by a vertical line (`|`). The distinction between
#'   them is more intuitive in [explore()], where the frozen columns do not respond
#'   to horizontal movements (with the `r` and `l` commands). The equivalent in
#'   this method is the `from_col` argument.
#' @name mclm_print
NULL

# Coercers =====================================================================

#' Coerce object to character
#' 
#' This method turns its argument `x`, or at least part of the information in it,
#' into a character vector.
#' 
#' @param x Object to coerce to character
#' @param ... Additional arguments
#' 
#' @return Object of class character
#' @export
#' @order 1
#' 
#' @examples 
#' (tks <- tokenize("The old man and the sea."))
#' as_character(tks) # turn 'tokens' object into character vector
#' as.character(tks) # alternative approach
#' 
#' as_character(1:10)
#' as.character(1:10)
#' 
#' regex <- re("(?xi) ^ .*")
#' as_character(regex) # turn 're' object into character vector
#' as.character(regex) # alternative approach
as_character <- function(x, ...) UseMethod("as_character")

#' @rdname as_character
#' @order 2 
as_character.default <- function(x, ...) as.character(x, ...)

#' Coerce object to a data frame
#' 
#' `as_data_frame()` is an alternative to [as.data.frame()]. A number of objects
#' in `r packageName()` can be turned into dataframes with one of these functions.
#' 
#' @param x Object to coerce to [data.frame].
#' @param row.names `NULL` or a character vector giving the rownames for the
#'   dataframe.
#' @param optional Logical. If `TRUE`, setting rownames and converting column
#'   names is optional (see [as.data.frame()]).
#' @param ... Additional arguments
#' 
#' 
#' @return Object of class [`data.frame`]
#' @export
#' @order 1
#' 
#' @examples 
#' # for an assoc_scores object ---------------------
#' a <- c(10,    30,    15,    1)
#' b <- c(200, 1000,  5000,  300)
#' c <- c(100,   14,    16,    4)
#' d <- c(300, 5000, 10000, 6000)
#' types <- c("four", "fictitious", "toy", "examples")
#' (scores <- assoc_abcd(a, b, c, d, types = types))
#' 
#' as.data.frame(scores)
#' as_data_frame(scores)
#' 
#' # for a conc object ------------------------------
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' as.data.frame(conc_data)
#' 
#' # for an fnames object ---------------------------
#' cwd_fnames <- as_fnames(c('file1', 'file2'))
#' as.data.frame(cwd_fnames)
#' 
#' # for a freqlist, types or tokens object ---------
#' toy_corpus <- "Once upon a time there was a tiny toy corpus.
#'   It consisted of three sentences. And it lived happily ever after."
#' (flist <- freqlist(toy_corpus, as_text = TRUE))
#' as.data.frame(flist)
#' 
#' (flist2 <- keep_re(flist, "^..?$"))
#' as.data.frame
#' 
#' (toks <- tokenize(toy_corpus))
#' as.data.frame(toks)
#' 
#' (toks <- tokenize(toy_corpus))
#' as.data.frame(toks)
as_data_frame <- function(x, row.names = NULL,
                          optional = FALSE, ...) UseMethod("as_data_frame")

#' @rdname as_data_frame
#' @order 2
as_data_frame.default <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}

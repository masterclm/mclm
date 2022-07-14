# We'll document the generics and add the methods instead of documenting the
# methods individually (since it's harder for people to know how to search...)

#' Interactively navigate through an object
#' 
#' This method only works in an interactive R session to open
#' 'exploration mode', in which the user can navigate through the
#' object `x` by means of brief commands. In 'exploration mode' the user can
#' ask of a list of available commands by keying in `?`, followed by `ENTER`.
#' The user can quiet 'exploration mode' by keying in `q`, followed by `ENTER`.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @inheritParams mclm_print
#' @param perl Boolean value. Whether or not the regular expressions used in the
#'   exploration session use the PERL flavour of regular expression.
#' @param use_clear Boolean. If `TRUE`, and if the feature is supported by the R
#'   environment, the console will be cleared in between all interactive steps
#'   in the exploration session.
#' @param ... Additional arguments.
#'
#' @return Invisibly, `x`.
#' @export
#' @order 1
explore <- function(x, ...) UseMethod("explore")

#' @rdname explore
#' @exportMethod 
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
#' @param invert Boolean vector of length one, which indicates whether the matches
#'   or the non-matches should be selected.
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
keep_pos <- function(x,
                     pos,
                     invert = FALSE,
                     ...) UseMethod("keep_pos")

#' @rdname keep_pos
#' @exportMethod 
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

#' @rdname keep_pos
#' @order 2
#' @exportMethod 
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
#' @param pattern Either an object of the class [re()]
#'   or a character vector of length one containing a regular expression.
#' @param perl Boolean vector of length one, which indicates whether or not
#'   `pattern` is treated as a PCRE flavour regular expression.
#'   The `perl` argument is only used if `pattern` is a regular character vector.
#'   If `pattern` is an object of the class [re()], then the
#'   `perl` argument is ignored, and the relevant information in the
#'   [re()] object `pattern`, viz. the value of `pattern$perl`, is
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
keep_re <- function(x,
                    pattern,
                    perl = TRUE,
                    invert = FALSE,
                    ...) UseMethod("keep_re")

#' @rdname keep_re
#' @exportMethod 
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

#' @rdname keep_re
#' @exportMethod 
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
#' @param types Either an object of the class `types` (see [types()] or [as_types()])
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
keep_types <- function(x,
                       types,
                       invert = FALSE,
                       ...) UseMethod("keep_types")

#' @rdname keep_types
#' @exportMethod 
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

#' @rdname keep_types
#' @order 2
#' @exportMethod 
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

#' keep_bool(tps, c(TRUE, FALSE))
#' drop_bool(tps, c(TRUE, FALSE))
keep_bool <- function(x,
                      bool,
                      invert = FALSE,
                      ...) UseMethod("keep_bool")

#' @rdname keep_bool
#' @exportMethod 
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

#' @rdname keep_bool
#' @exportMethod 
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
#' - providing a [re()] object is equivalent to calling [keep_re()],
#' - providing a numeric vector is equivalent to calling [keep_pos()],
#' - providing a logical vector is equivalent to calling [keep_bool()],
#' - providing a [types()] object or a character vector is equivalent to calling [keep_types()].
#' 
#' When the notation `x[i, ...]` is used, it is also possible to set the `invert`
#' argument to `TRUE` (which then is one of the additional arguments in `...`).
#' This `invert` argument then serves the same purpose as the `invert` argument
#' in the `keep_` methods, turning it into a `drop_` method.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param ... Additional arguments.
#' @param invert Boolean vector of length one, which indicates whether the matches
#'   or the non-matches should be selected.
#' @param i Selection criterion; depending on its class, it behaves differently.
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
#' @exportS3Method n_tokens default
#' @export
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
#' @exportS3Method n_types default
#' @export
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

#' @rdname stubs
#' @exportS3Method type_names default
#' @export
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

#' @rdname stubs
#' @exportS3Method tot_n_tokens default
#' @export
tot_n_tokens.default <- function(x) NULL

#' @rdname tot_n_tokens
#' @order 2
#' @exportMethod 
"tot_n_tokens<-" <- function(x, value) UseMethod("tot_n_tokens<-")

#' @rdname stubs
#' @exportS3Method `tot_n_tokens<-` default
#' @export
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
#' @param with_names Boolean. Whether or not the items in the output should
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

#' @rdname stubs
#' @exportS3Method rog_ranks default
#' @export
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
#' to be confused with the base R function [base::rank()]. There are two
#' important differences.
#' 
#' First, the base R function [base::rank()] always ranks items from low values to
#' high values and [ranks()] ranks from high
#' frequency items to low frequency items.
#' 
#' Second, the base R function [base::rank()] allows the user to choose among
#' a number of different ways to handle ties.
#' In contrast, [ranks()] always handles ties
#' in the same way. More specifically, items with identical frequencies
#' are always ranked in alphabetical order.
#' 
#' In other words, the base R function [base::rank()] is a flexible tool that
#' supports a number of different ranking methods that are commonly used in
#' statistics. In contrast, [ranks()] is a
#' rigid tool that supports only one type of ranking, which is a type of
#' ranking that is atypical from a statistics point of view, but is commonly
#' used in linguistic frequency lists. Also, it is designed to be unaffected
#' by the order of the items in the frequency list.
#'
#' @param x An object of any of the classes for which the method is implemented.
#' @param with_names Boolean. Whether or not the items in the output should
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

#' @rdname stubs
#' @exportS3Method ranks default
#' @export
ranks.default <- function(x, ...) NULL

trunc_at <- function(x, pattern, ...) UseMethod("trunc_at")

trunc_at.default <- function(x, pattern, ...) invisible(x)

as_numeric <- function(x, ...) UseMethod("as_numeric")

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
#'   If `x` is an object of class [`assoc_scores`][assoc_scores()], a column name
#'   or vector of column names may be provided instead.
#' @param extra Extra settings, as an [base::environment]. Arguments defined here
#'   take precendence over other arguments. For instance, if `extra$from_col` is
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

# Not implemented ==============================================================

as_character <- function(x, ...) UseMethod("as_character")

as_character.default <- function(x, ...) as.character(x, ...)

as_data_frame <- function(x, row.names = NULL,
                          optional = FALSE, ...) UseMethod("as_data_frame")

as_data_frame.default <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}

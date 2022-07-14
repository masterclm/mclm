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
#' @param n Maximum number of items in the object to be shown at once.
#' @param from Index of the first item to be shown at the beginning of the
#'   interactive session.
#' @param from_col Index of the first column to be displayed (among all selected
#'   columns, including frozen columns).
#' @param perl Boolean value. Whether or not the regular expressions used in the
#'   exploration session use the PERL flavour of regular expression.
#' @param sort_order Order in which the items are to be printed. If `"alpha"` is
#'   given, the items will be sorted alphabetically; `"none"` indicates that no
#'   sorting should be done. A column name can also be provided.
#' @param use_clear Boolean. If `TRUE`, and if the feature is supported by the R
#'   environment, the console will be cleared in between all interactive steps
#'   in the exploration session.
#' @param ... Additional arguments.
#'
#' @return Invisibly, `x`.
#' @md
#' @export
#' @order 1
explore <- function(x, ...) UseMethod("explore")

explore.default <- function(x, ...) invisible(x)

# Subsetters ===================================================================

keep_pos <- function(x,
                     pos,
                     invert = FALSE,
                     ...) UseMethod("keep_pos")

keep_pos.default <- function(x,
                             pos,
                             invert = FALSE,
                             ...) {
  warning("unsupported type of x; simply returning x")
  x
}

keep_re <- function(x,
                    pattern,
                    perl = TRUE,
                    invert = FALSE,
                    ...) UseMethod("keep_re")

keep_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            invert = FALSE,
                            ...) {
  warning("unsupported type of x; simply returning x")
  x
}

keep_types <- function(x,
                       types,
                       invert = FALSE,
                       ...) UseMethod("keep_types")

keep_types.default <- function(x,
                               types,
                               invert = FALSE,
                               ...) {
  warning("unsupported type of x; simply returning x")
  x
}

keep_bool <- function(x,
                      bool,
                      invert = FALSE,
                      ...) UseMethod("keep_bool")

keep_bool.default <- function(x,
                              bool,
                              invert = FALSE,
                              ...) {
  warning("unsupported type of x; simply returning x")
  x
}

drop_pos <- function(x,
                     pos,
                     ...) UseMethod("drop_pos")

drop_pos.default <- function(x,
                             pos,
                             ...) {
  warning("unsupported type of x; simply returning x")
  x
}

drop_re <- function(x,
                    pattern,
                    perl = TRUE,
                    ...) UseMethod("drop_re")

drop_re.default <- function(x,
                            pattern,
                            perl = TRUE,
                            ...) {
  warning("unsupported type of x; simply returning x")
  x
}
drop_types <- function(x,
                       types,
                       ...) UseMethod("drop_types")

drop_types.default <- function(x,
                               types,
                               ...) {
  warning("unsupported type of x; simply returning x")
  x
}
drop_bool <- function(x,
                      bool,
                      ...) UseMethod("drop_bool")

drop_bool.default <- function(x,
                              bool,
                              ...) {
  warning("unsupported type of x; simply returning x")
  x
}

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
#' @md
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

tot_n_tokens.default <- function(x) NULL

"tot_n_tokens<-" <- function(x, value) UseMethod("tot_n_tokens<-")

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

orig_ranks.default <- function(x, ...) NULL

"orig_ranks<-" <- function(x, value) UseMethod("orig_ranks<-")

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
#' @md
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

ranks.default <- function(x, ...) NULL

trunc_at <- function(x, pattern, ...) UseMethod("trunc_at")

trunc_at.default <- function(x, pattern, ...) invisible(x)

as_numeric <- function(x, ...) UseMethod("as_numeric")

as_numeric.default <- function(x, ...) as.numeric(x, ...)

# Not implemented ================================================================

as_character <- function(x, ...) UseMethod("as_character")

as_character.default <- function(x, ...) as.character(x, ...)

as_data_frame <- function(x, row.names = NULL,
                          optional = FALSE, ...) UseMethod("as_data_frame")

as_data_frame.default <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}

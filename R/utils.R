# For slma() ===================================================================

adhoc_min <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    min(x, na.rm = TRUE)
  }
}

adhoc_max <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    max(x, na.rm = TRUE)
  }
}

adhoc_sum <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    sum(x, na.rm = TRUE)
  }
}

# Unicode related issues =======================================================

# Called by read_fnames() and read_types()
restore_unicode <- function(x) {
  x <- gsub("<U\\+([0-9A-F]{4})>", "\\\\u\\1", x, perl = TRUE)
  stringi::stri_unescape_unicode(x)
}

# Matrix manipulation functions ================================================

#' Drop empty rows and columns from a matrix
#' 
#' With `x` a matrix containing frequency counts, `drop_empty_rc` makes
#' a copy of `x` from which the all-zero rows and all-zero columns are removed.
#' No checks are performed by this function.
#' 
#' This is just a convenience function. It is identical to, and implemented as,
#' `x[rowSums(x) > 0, colSums(x) > 0, drop = FALSE]`.
#'
#' @param x A matrix, assumed to contain frequency counts.
#'
#' @return Matrix, with all-zero rows and columns removed.
#' @export
#' 
#' @examples
#' # first example
#' m <- matrix(nrow = 3, byrow = TRUE,
#'             dimnames = list(c('r1','r2','r3'),
#'                            c('c1','c2','c3')),
#'            c(10, 0, 4,
#'              0, 0, 0,
#'              5, 0, 7))
#' 
#' m
#' m2 <- drop_empty_rc(m)
#' m2
#' 
#' ## second example
#' m <- matrix(nrow = 3, byrow = TRUE,
#'            dimnames = list(c('r1','r2','r3'),
#'                           c('c1','c2','c3')),
#'            c(0, 0, 4,
#'              0, 0, 0,
#'              0, 0, 7))
#' m
#' m2 <- drop_empty_rc(m)
#' m2
#' 
#' ## third example
#' m <- matrix(nrow = 3, byrow = TRUE,
#'             dimnames = list(c('r1','r2','r3'),
#'                             c('c1','c2','c3')),
#'            c(0, 0, 0,
#'              0, 0, 0,
#'              0, 0, 0))
#' m
#' m2 <- drop_empty_rc(m)
#' m2 
drop_empty_rc <- function(x) {
  x[rowSums(x) > 0, colSums(x) > 0, drop = FALSE]
}

# Verbose functions ============================================================

# Print to console to indicate progress in slma()
cat_if_verbose <- function(x, verbose = TRUE) {
  if (verbose) {
    cat(x)
    utils::flush.console()
  }
}

# Print a dot while computing a measure in assoc_abcd()
show_dot <- function(show_dots = FALSE) {
  if (show_dots) {
    cat(".")
    utils::flush.console()
  }
  invisible(show_dots)
}

## Functions for explore() methods =============================================

char_line <- function(char = "_", width = NULL, EOL = TRUE) {
  if (is.null(width)) {
    width <- max(0, getOption("width") - 4) # margin of 4 'just in case'
  }
  line <- paste0(rep(char, width), collapse = "")
  if (EOL) {
    line <- paste0(line, "\n")
  }
  line
}

clear_console <- function() {
  if (Sys.getenv("RSTUDIO_USER_IDENTITY") != "") {
    cat('\f')       ## OK in RStudio on Windows, macOS en Ubuntu
  } else if (.Platform["GUI"] == "X11") {
    system("clear") ## OK on macOS and Ubuntu terminals
                    ## OK on ssh link to r2d2
  }
  invisible(NULL)
}

settings <- function(...) {
  result <- new.env(parent = emptyenv())
  args <- list(...)
  features <- names(args)
  features <- features[nchar(features) > 0]
  for (feature in features) {
    assign(feature, args[[feature]], envir = result)
  }
  result
}

## Functions for (also) print() methods ========================================

mclm_style_very_dim <- function(x) {
  crayon::style(x, as = grDevices::rgb(0.7, 0.7, 0.7), bg = NULL)
}

mclm_style_dim <- function(x) {
  crayon::silver(x)
}

#' Add left padding
#' 
#' Motivation: stringr::str_pad and string::stri_pad_left appear to mess up in
#' certain contexts, e.g.
#'   `Sys.setlocale(category = "LC_ALL", locale = "Greek")`
#'   `stringr::str_pad(c("Λορεμ", "ιπσθμ", "δολορ", "σιτ", "αμετ"), 20)`
#'
#' @param x Character string
#' @param width Desired width of complete text
#' @param pad Character to use for padding
#' @param nchar_x Number of characters of text. If `NULL`, `nchar(x)`.
#'
#' @return Character string with left padding.
#' @noRd
mclm_pad_left <- function(x, width, pad = " ", nchar_x = NULL) {
  if (is.null(nchar_x)) {
    nchar_x <- nchar(x)
  }
  len_needed <- pmax(0, width - nchar_x)
  paste0(unlist(Map(rep_str, rep(pad, length(x)), len_needed)), x)
}

#' Apply styles to string
#' 
#' This function is called by [show_matches()].
#'
#' @param x Character vector
#' @param styles Dataframe with four columns: `from`, `to`, `as` and `bg`.
#'   The values in `from` and `to` identify non-empty areas in `x`: `from` must
#'   always be smaller than `to`.
#'   The values in `as` and `bg` can either be the name of a style or `NA` - in
#'   the former case they will be used as argument for [crayon::style()]; `NA`
#'   values will be turned to `NULL` before providing them to [crayon::style()].
#'   
#'   In addition, the rows should be sorted by `list(styles$from, styles$to)` and
#'   describe areas that never overlap partially: either they don't overlap at
#'   all or they are completely nested.
#'
#' @return Styled character vector
#' @noRd
apply_styles <- function(x, styles) {
  if (nrow(styles) == 0) {
    return(x)
  }
  ## -- analyse current style, i.e. styles[1, ], and remove it from styles
  cur_from   <- styles$from[1]
  cur_to     <- styles$to[1]
  cur_as     <- styles$as[1]; if (is.na(cur_as)) cur_as <- NULL 
  cur_bg     <- styles$bg[1]; if (is.na(cur_bg)) cur_bg <- NULL  
  styles     <- styles[-1, , drop = FALSE]
  
  ## -- process area before current style area
  x_before   <- substr(x, 1, cur_from - 1)
  
  ## -- process area to which current style applies
  x_inside   <- substr(x, cur_from, cur_to)
  styles_inside_bool <- styles$from <= cur_to 
  styles_inside  <- styles[styles_inside_bool, , drop = FALSE]
  
  if (nrow(styles_inside) > 0) {
    styles_inside$from <- styles_inside$from - cur_from + 1
    styles_inside$to   <- styles_inside$to - cur_from + 1
    x_inside <- apply_styles(x_inside, styles_inside)
  }
  x_inside <- crayon::style(x_inside, as = cur_as, bg = cur_bg)
  
  ## -- process area after current style area
  x_after <- substr(x, cur_to + 1, nchar(x))    
  styles_after_bool <- styles$from > cur_to 
  styles_after  <- styles[styles_after_bool, , drop = FALSE]
  if (nrow(styles_after) > 0) {
    styles_after$from  <- styles_after$from - cur_to
    styles_after$to    <- styles_after$to - cur_to
    x_after <- apply_styles(x_after, styles_after)
  }
  ## -- return result
  paste0(x_before, x_inside, x_after)
}

show_matches <- function(x, pattern, ...) {
  matches <- stringr::str_locate_all(x, pattern)
  styles <- lapply(matches, function(x) {
                              dplyr::tibble(
                                     from = x[, 1],
                                     to = x[, 2],
                                     as = rep(c("green", "blue"),
                                              length = nrow(x)),
                                     bg = rep(NA, length = nrow(x))) })
  unlist(Map(apply_styles, x, styles))
}

#' Clean up the use of whitespace in a character vector
#' 
#' The function [cleanup_spaces()] takes a character vector and input and turns
#' any uninterrupted stretch of whitespace characters into one single space character.
#' Moreover, it can also *remove* leading whitespace and trailing whitespace.
#'
#' @param x Character vector.
#' @param remove_leading Logical. If `TRUE`, leading whitespace will be removed.
#' @param remove_trailing Logical. If `TRUE`, trailing whitespace will be removed.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' txt <- "  A \\t  small      example \\n with redundant whitespace    "
#' cleanup_spaces(txt)
#' cleanup_spaces(txt, remove_leading = FALSE, remove_trailing = FALSE)
cleanup_spaces <- function(x,
                           remove_leading = TRUE,
                           remove_trailing = TRUE) {
  x <- gsub("\\s+", " ", x, perl = TRUE)
  if (remove_leading) {
    x <- gsub("^[ ]", "", x, perl = TRUE)
  }
  if (remove_trailing) {
    x <- gsub("[ ]$", "", x, perl = TRUE)
  }
  x
}

#' Drop XML tags from character string
#' 
#' This function takes a character vector and retuns a copy from which all
#' XML-like tags have been removed. Moreover, if `half_tags_too = TRUE`
#' any half tag at the beginning or end of `x` is also removed.
#' 
#' This function is not XML-aware. It uses a very simple definition of what
#' counts as a tag. More specifically, any character sequence starting with
#' `<` and ending with `>` is considered a 'tag'; inside such a tag, between
#' `<` and `>`, `drop_tags()` accepts any sequence of zero or more characters.
#' 
#' @param x String with xml tag
#' @param half_tags_too Logical. Whether tags with only opening/closing
#'   bracket should also be removed.
#' @return Character string
#' @export
#' 
#' @examples
#' xml_snippet <- "id='3'/><w pos='Det'>An</w> <w pos='N'>example</w> <w"
#' drop_tags(xml_snippet)
#' drop_tags(xml_snippet, half_tags_too = FALSE)
drop_tags <- function(x, half_tags_too = TRUE) {
  # Called by print_kwic()
  if (half_tags_too) {
    x <- gsub("^[^<]*>|<[^>]*>|<[^>]*$", "", x, perl = TRUE)
  } else {
    x <- gsub("<[^>]*>", "", x, perl = TRUE)
  }
  x
}

#' Repeat a string and paste
#' 
#' Used by [mclm_pad_left()].
#' 
#' @param x Character string
#' @param n Times to repeat
#' @return Character string
#' @noRd
rep_str <- function(x, n) {
  paste(rep(x, n), collapse = "")
}

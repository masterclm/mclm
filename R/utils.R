# Distance measures ============================================================


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

# string manipulation functions ================================================

#' Remove spaces from character string
#'
#' @param x Character string.
#' @param remove_leading Whether to remove leading spaces.
#' @param remove_trailing Wheather to remove trailing spaces.
#'
#' @return Character string
#' @noRd
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

# Drop XML tags from character string
drop_tags <- function(x, half_tags_too = TRUE) {
   if (half_tags_too) {
      x <- gsub("^[^<]*>|<[^>]*>|<[^>]*$", "", x, perl = TRUE)
   } else {
      x <- gsub("<[^>]*>", "", x, perl = TRUE)
   }
   x
}

# Repeat a string and paste
rep_str <- function(x, n) {
  paste(rep(x, n), collapse = "")
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

# unicode related issues =======================================================

restore_unicode <- function(x) {
  x <- gsub("<U\\+([0-9A-F]{4})>", "\\\\u\\1", x, perl = TRUE)
  stringi::stri_unescape_unicode(x)
}

to_utf8 <- function(x) {
  stringi::stri_encode(x, "UTF-8")
}

# matrix manipulation functions ================================================

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

# console related functions ====================================================

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

# ngram related functions ======================================================

# TODO: Some of the functions below either (i) will have to be 'translated'...
#    from a 'for-loop' approach to an 'lapply' approach or (ii) will have to be
#    adapted so that they use mutable objects (e.g. environments).

# TODO: Build (real) skip-gram routine


list_paste <- function(x, sep = "_") {
  if (length(x) == 0) {
    character(0)
  } else if (length(x) == 1) {
    x[[1]]
  } else {
    paste(x[[1]], list_paste(x[-1], sep = sep), sep = sep)
  }
}

list_paste_open <- function(x,
                            first_pos = 1,
                            sep = "_",
                            open_pos = numeric(0),
                            open = "[]") {
  if (length(x) == 0) {
    character(0)
  } else {
    if (first_pos %in% open_pos) {
      car <- rep(open, length(x[[1]]))
    } else {
      car <- x[[1]]
    }
    if (length(x) == 1) {
      car
    } else {
      paste(car,
            list_paste_open(x[-1], first_pos = first_pos + 1, sep = sep,
                             open_pos = open_pos, open = open),
            sep = sep)
      
    }
  }
}

build_open_pos <- function(ngram_size, n_open) {
  result <- list()
  if ((n_open > 0) && (ngram_size - n_open > 1)) {
    result <- as.list(2:(ngram_size - n_open))
    n_open <- n_open - 1
  }
  if (length(result) > 0) {
    while (n_open > 0) {
      old_result <- result
      result <- list()
      for (item in old_result) {
        last <- item[length(item)]
        for (i in (last + 1):(ngram_size - n_open)) {
           result[[length(result) + 1]] <- c(item, i)
        }
      }
      n_open <- n_open - 1
    }
  }
  result
}

#' Build an n-gram
#'
#' @param x An object of class [`tokens`]
#' @param ngram_size Number of items in the ngram.
#' @param max_skip If larger than 1, [skipgrams()] wil be called.
#' @param sep Character vector to join the elements in the ngram/skipgram.
#' @param n_open Number of open slots.
#' @param open Character vector used to represent open slots.
#'
#' @return Character vector
#' @noRd
build_ngrams <- function(x,
                         ngram_size = 3,
                         max_skip = 0,
                         sep = "_",
                         n_open = 0,
                         open = "[]") {
  result <- character(0)
  if (length(x) >= ngram_size) {
    # -- skipgrams --
    if (max_skip > 0) {
      result <- skipgrams(x, ngram_size = ngram_size,
                          max_skip = max_skip, sep = sep) 
    # -- regular ngrams --
    } else {
      parts <- vector(mode = "list", length = ngram_size)
      for (i in 1:ngram_size) {
        parts[[i]] <- x[i:(i + length(x) - ngram_size)]
      }
      # -- without open slots --
      if (n_open == 0) {
        result <- list_paste(parts, sep = sep)
      # -- with open slots --
      } else {
        open_pos_list <- build_open_pos(ngram_size, n_open)
        for (open_pos in open_pos_list) {
          result <- c(result,
                      list_paste_open(parts,
                                      open_pos = open_pos,
                                      sep = sep,
                                      open = open))
        }
      }
    }
  }
  result
}

## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
##
## Different sources of information:
##     .Platform["OS.type"]   ## e.g. "windows"
##                            ##      "unix" (ook op mac)
##                            ##      "unix" (ook in Ubuntu terminal)
##                            ##      "unix" (ook in ssh terminal naar r2d2)
##     .Platform["GUI"]       ## e.g. "RStudio" (op alle OSen)
##                            ##      "Rgui"  (default R GUI on Windows)
##                            ##      "AQUA" (R.app on macOS)
##                            ##      "X11" (Terminal op Mac)
##                            ##      "X11" (Terminal op Ubuntu)
##                            ##      "X11" (ssh terminal naar r2d2)
##     R.version["os"]        ## e.g. "mingw32"
##                            ##      "os darwin13.4.0"
##                            ##      "os linux-gnu" 
##     Sys.info()["sysname"]  ## e.g. "Windows"
##                            ##      "Darwin"
##                            ##      "Linux"

## system("clear") works in macOS Terminal and in Ubuntu Terminal

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "macOS"
  } else { 
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "macOS"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

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

mclm_style_very_dim <- function(x) {
  crayon::style(x, as = grDevices::rgb(0.7, 0.7, 0.7), bg = NULL)
}

mclm_style_dim <- function(x) {
  crayon::silver(x)
}

mclm_style_inverse <- function(x) {
  crayon::inverse(x)
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

apply_styles <- function(x, styles) {
  ## - x must be a character vector
  ## - styles must be a data frame with four columns,
  ##   viz. styles$from, styles$to, styles$as, and styles$bg. The values in
  ##   styles$from and styles$to identify non-empty areas in x. It must
  ##   always be the case that styles$from is smaller than styles$to.
  ##   The values in styles$as and styles$bg can either be the name of a
  ##   style or the value NA. These values will be used as argument for
  ##   crayon::style(string, as = NULL, bg = NULL). NA values will be turned
  ##   into NULL before handing them over to crayon::style().
  ## - There are two additional assumptions related to the rows in styles:
  ##   (i)   be sorted by list(styles$from, styles$to)
  ##   (ii)  describe areas that never overlap partially; i.e. two areas
  ##         either do not overlap at all, or, if they do overlap, then one
  ##         of the two areas must be completely nested in the other one.
  if (nrow(styles) == 0) {
    return(x)
  } else {
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
    return(paste0(x_before, x_inside, x_after))
  }
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

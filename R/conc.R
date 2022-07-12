# Create or coerce to class ====================================================

#' Build a concordance for the matches of a regex
#' 
#' Build a concordance for the matches of a regular expresion. The result is a
#' dataset that can be written to a file with the function \code{\link{write_conc}}.
#' It mimics the behavior of the concordance tool in the program AntConc.
#' 
#' In order to make sure that the columns \code{left}, \code{match},
#' and \code{right} in the output of \code{conc} do not contain any TAB or NEWLINE
#' characters, whitespace in these items is being 'normalized'.
#' More particularly, each stretch of whitespace, i.e. each  uninterrupted
#' sequences of whitespace characters, is replaced by  a single SPACE character.
#' 
#' The values in the items the \code{glob_id} and \code{id} in the output
#' of \code{conc} are always identical in a dataset that is the output of the
#' function \code{conc}. The item  \code{glob_id} only becomes useful when later,
#' for instance, one wants to merge two datasets.#' 
#'
#' @param x A character vector determining which text is to be used as corpus.
#'   
#'   If \code{as_text = TRUE}, \code{x} is treated as the actual text to be used
#'   as corpus.
#'   
#'   If \code{as_text = FALSE} (the default), \code{x} is trated as a vector of
#'   filenames, interpreted as the names of the corpus files that contain the
#'   actual corpus data.
#' @param pattern Character string containing the regular expression that serves
#'   as search term for the concordancer.
#' @param c_left Number. How many characters to the left of each match must be
#'   included in the result as left co-text of the match.
#' @param c_right Number. How many characters to the right of each match must be
#'   included in the result as right co-text of the match.
#' @param perl If \code{TRUE}, \code{pattern} is treated as a PCRE flavor regular
#'   expression. Otherwise, \code{pattern} is treated as a regular expression in R's
#'   default flavor of regular expression.
#' @param re_drop_line Character vector or \code{NULL}. If \code{NULL}, the argument
#'   is ignored.
#'   Otherwise, lines in \code{x} containing a match for \code{re_drop_line} are
#'   treated as not belonging to the corpus and are excluded from the results.
#' @param line_glue Character vector or \code{NULL}. If \code{NULL}, the argument
#'   is ignored.
#'   Otherwise, all lines in the corpus are glued together in one character
#'   vector of length 1, with the string \code{line_glue} pasted in between
#'   consecutive lines.
#'   The value of \code{line_glue} can also be equal to the empty string (\code{""}).
#'   The 'line_glue' operation is conducted immediately after the 'drop line' operation.
#' @param re_cut_area Character vector or \code{NULL}. If \code{NULL}, the argument
#'   is ignored.
#'   Otherwise, all matches in the corpus are 'cut out' of the text prior to the
#'   identification of the tokens in the text (and are therefore not taken into
#'   account when identifying tokens).
#'   The 'cut area' operation is conducted immediately after the 'line glue' operation.
#' @param file_encoding File encoding for reading each corpus file. Ignored if
#'   \code{as_text = TRUE}. Otherwise, it must be a character vector of length one
#'   (in which case the same encoding is used for all files) or with the same
#'   length as \code{x} (in which case each file can have a different encoding).
#' @param as_text Boolean value.
#'   If \code{TRUE}, the content of \code{x} is treated
#'   as the actual text of the corpus (with each item within \code{x} treated as
#'   a separate 'document in RAM').
#'   
#'   If \code{FALSE}, \code{x} is treated as a vector of filenames, interpreted
#'   as the namse of the corpus files with the actual corpus data.
#'
#' @return Object of class \code{conc}, a kind of data frame with as its rows
#'   the matches and with the following columns:
#'   \describe{
#'   \item{glob_id}{Number indicating the position of the match in the
#'     overall list of matches.}
#'   \item{id}{Number indicating the position of the match in the list of matches
#'     for one specific query.}
#'   \item{source}{Either the filename of the file in which the match was found
#'     (in case of the setting \code{as_text = FALSE}), or the string '-'
#'     (in case of the setting \code{as_text = TRUE}).}
#'   \item{left}{The lefthandside co-text of each match.}
#'   \item{match}{The actual match.}
#'   \item{right}{The righthandside co-text of each match.}
#'   }  
#' @export
#'
#' @examples
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' print_kwic(conc_data)
conc <-    function(x,
                    pattern,
                    c_left = 200,
                    c_right = 200,
                    perl = TRUE,
                    re_drop_line = NULL,
                    line_glue = "\n",
                    re_cut_area = NULL,
                    file_encoding = "UTF-8",
                    as_text = FALSE) {
  n_texts <- length(x)
  list_source <- vector("list", n_texts)
  list_left <- vector("list", n_texts)
  list_hits <- vector("list", n_texts)
  list_right <- vector("list", n_texts)
  if (length(file_encoding) < n_texts) {
    file_encoding <- rep(file_encoding, length = n_texts)
  }   
  for (i in seq_along(x)) {
    if (as_text) {
      text <- x[i]
    } else {
      fname <- x[i]
      text <- read_txt(fname, line_glue = line_glue,
                       file_encoding = file_encoding[i])
    }
    
    # (further) split into lines
    text <- unlist(strsplit(text, split = "\n"))
    
    # drop lines if needed
    if (!is.null(re_drop_line) && !is.na(re_drop_line[1])) {
      text <- x[grep(re_drop_line[1], text, perl = perl, invert = TRUE)]
    }
    
    # paste lines in long line if needed
    if (!is.null(line_glue) && !is.na(line_glue[1])) {
      text <- paste(text, collapse = line_glue[1])
    }
    
    # drop uninterestion regions if needed
    if (!is.null(re_cut_area) && !is.na(re_cut_area[1])) {
      text <- gsub(re_cut_area[1], "", text, perl = perl)
    }
    
    m <- gregexpr(pattern, text, perl = perl)[[1]]
    start <- as.numeric(m)
    stop <- start + attr(m, "match.length") - 1
    left <- vector("character", 0)
    hits <- vector("character", 0)
    right <- vector("character", 0)
    if (start[1] > 0) { # if there are matches
      left  <- substr(rep(text, length(start)), start - c_left, start - 1)
      hits  <- substr(rep(text, length(start)), start, stop)
      right <- substr(rep(text, length(start)), stop + 1, stop + c_right)
    }
    if (as_text) {
      list_source[[i]] <- rep("-", length(left))
    } else {
      list_source[[i]] <- rep(fname, length(left))        
    }
    list_left[[i]] <- left
    list_hits[[i]] <- hits
    list_right[[i]] <- right      
  }
  src <- unlist(list_source)
  lft <- unlist(list_left)
  hts <- unlist(list_hits)
  rgt <- unlist(list_right)
  if (length(src) > 0) {
    gid <- id <- 1:length(src)
    lft <- cleanup_spaces(lft, remove_leading = FALSE, remove_trailing = FALSE) 
    hts <- cleanup_spaces(hts, remove_leading = FALSE, remove_trailing = FALSE) 
    rgt <- cleanup_spaces(rgt, remove_leading = FALSE, remove_trailing = FALSE) 
  } else {
    gid <- id <- numeric(0)
  }
  df <- data.frame(glob_id = gid,
                   id = id,
                   source = src,
                   left = lft,
                   match = hts,
                   right = rgt,
                   stringsAsFactors = FALSE)
  class(df) <- c("conc", class(df))
  df
}

#' Coerce data frame to a concordance object
#' 
#' Coerce a data frame to an object of the class \code{conc}.
#'
#' @param x A data frame.
#' @param left The name of the column in \code{x} that contains the left co-text
#'   of the concordance. Is \code{is.na(left)}, then this column is assumed
#'   to have the name \code{"left"}.
#' @param match The name of the column in \code{x} that contains the match
#'   of the concordance. Is \code{is.na(match)}, then this column is assumed
#'   to have the name \code{"match"}.
#' @param right The name of the column in \code{x} that contains the right co-text
#'   of the concordance. Is \code{is.na(right)}, then this column is assumed
#'   to have the name \code{"right"}.
#' @param keep_original Boolean value. If the values of
#'   \code{left}, \code{match} or \code{right} are not \code{NA}, should
#'   the original names of those columns be kept in the \code{conc} object.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{conc}.
#' @export
#'
#' @examples
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' df <- as.data.frame(conc_data)
#' as_conc(df)
as_conc <- function(x,
                    left = NA,
                    match = NA,
                    right = NA,
                    keep_original = FALSE,
                    ...) {
  if (!"data.frame" %in% class(x)) {
    stop("x must be of class \"data.frame\"")
  }
  d <- x
  class(d) <- c("conc", setdiff(class(d), "conc"))
  # ==
  if (! is.na(left) && left != "left") {
    if (is.null(d[[left]])) {
      stop(paste0("the object 'x' does not have a column ", left, "'"))
    }
    d[["left"]] <- as.character(d[[left]])
    if (! keep_original) {
      d[[left]] <- NULL 
    }    
  } else {
    if (is.null(d[["left"]])) {
      stop("the object 'x' does not have a column 'left'")
    }
    if (! is.character(d[["left"]])) {
      d[["left"]] <- as.character(d[["left"]])
    }  
  } 
  # ==
  if (! is.na(match) && match != "match") {
    if (is.null(d[[match]])) {
      stop(paste0("the object 'x' does not have a column ", match, "'"))
    }
    d[["match"]] <- as.character(d[[match]])
    if (! keep_original) {
      d[[match]] <- NULL 
    }    
  } else {
    if (is.null(d[["match"]])) {
      stop("the object 'x' does not have a column 'match'")
    }
    if (! is.character(d[["match"]])) {
      d[["match"]] <- as.character(d[["match"]])
    }  
  } 
  # ==
  if (! is.na(right) && right != "right") {
    if (is.null(d[[right]])) {
      stop(paste0("the object 'x' does not have a column ", right, "'"))
    }
    d[["right"]] <- as.character(d[[right]])
    if (! keep_original) {
      d[[right]] <- NULL 
    }    
  } else {
    if (is.null(d[["right"]])) {
      stop("the object 'x' does not have a column 'right'")
    }
    if (! is.character(d[["right"]])) {
      d[["right"]] <- as.character(d[["right"]])
    }  
  } 
  # ==
  d
}

# S3 methods from mclm =========================================================

#' Interactively navigate through concordance list
#' 
#' This method only works in an interactive R session to open
#' 'exploration mode', in which the user can navigate through the \code{conc}
#' object \code{x} by means of brief commands. In 'exploration mode' the user can
#' ask of a list of available commands by keying in \code{?}, followed by ENTER.
#' The user can quiet 'exploration mode' by keying in \code{q}, followed by ENTER.
#'
#' @param x Object of class \code{conc}.
#' @param n Maximum number of items to show at once.
#' @param from Index of the first item to be shown in the console at the beginning.
#' @param use_clear Boolean value. If \code{TRUE} and the feature is supported by
#'   the R environment, the console will be cleared in between all the interactive
#'   steps in the exploration session.
#' @param ... Additional arguments.
#'
#' @return Invisibly, \code{x}.
#' @export
#' @exportS3Method explore conc
explore.conc <- function(x,
                         n = 20,
                         from = 1,
                         use_clear = TRUE,
                         ...) {
  if (interactive()) {
    length_x <- nrow(x)                        # n items in x
    
    cur_command <- "i"                         # "idle" (no change of state)
    cur_com_verb <- substr(cur_command, 1, 1)  # actual command 
    cur_regex <- ".*"                          # last regex that was used
    cur_hits <- numeric(0)                     # ids of hits for last regex
    # ---- create and initialize printing settings --
    print_extra <- settings()                  
    
    while (cur_com_verb != "q") {
      ## -- initialize printing settings --
      assign("type_regex", NULL, envir = print_extra)
      ## -- prepare console --
      if (use_clear) { clear_console() }
      cat(mclm_style_dim(char_line()))
      cat("\n")
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
        } else if (cur_com_verb == "g") { ## g stands for '[g]o to item'
          old_from <- from
          tryCatch(from <- as.integer(substr(cur_command, 2,
                                             nchar(cur_command))),
                   error = function(e) from <- old_from)
          from <- max(1, min(from, length_x))
        }
        print_kwic(x, 
                   n = n,
                   from = from,
                   ...)
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

# S3 methods from other packages ===============================================

#' Print a concordance-based data frame.
#' 
#' Print an object of class \code{conc}.
#'
#' @param x An object of class \code{conc}.
#' @param n Number of items to print in the KWIC display.
#' @param ... Additional arguments for \code{\link{print_kwic}}.
#'
#' @return Invisibly, \code{x}.
#' @export
#' @exportS3Method print conc
#' @seealso \code{\link{print_kwic}}
#'
#' @examples
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' print_kwic(conc_data)
#' print(conc_data, n = 3)
print.conc <- function(x, n = 30, ...) {
  if (! "conc" %in% class(x)) {
    stop("the argument 'x' must be an object of the class 'conc'")
  } 
  cat("Concordance-based data frame (number of observations: ",
      nrow(x),
      ")\n",
      sep = "")
  if ((n > 0) && (nrow(x) > 0)) {
    print_kwic(x, n = n, ...)
  }
  if (n < nrow(x)) cat("...\n")
  cat("\nThis data frame has ")
  cat(ncol(x), ifelse(ncol(x) > 1, "columns:\n", "column:\n"))
  if (ncol(x) > 0) {
    df_names <- data.frame(column = names(x))
    print(df_names)
  }
  invisible(x)
}

#' Coerce concordance to data frame
#' 
#' Coerce object of class \code{conc} to \code{data.frame}.
#'
#' @param x An object of class \code{conc}.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{data.frame}.
#' @export
#' @exportS3Method as.data.frame conc
#' @seealso \code{\link{as_tibble.conc}}
#'
#' @examples
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' as.data.frame(conc_data)
as.data.frame.conc <- function(x, ...) {
  if (!"conc" %in% class(x)) {
    stop("x must be of class \"conc\"")
  }    
  d <- x
  class(d) <- setdiff(class(d), "conc")
  d
}

#' Coerce concordance to tibble
#' 
#' Coerce an object of class \code{conc} to \code{\link[tibble]{tibble}}.
#'
#' @param x Object of class \code{conc}.
#' @param ... Additional arguments.
#'
#' @return Object of class \code{\link[tibble]{tibble}}.
#' @export
#' @exportS3Method tibble::as_tibble conc
#'
#' @examples
#' (conc_data <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' as_tibble(conc_data)
as_tibble.conc <- function(x, ...) {
  if (!"conc" %in% class(x)) {
    stop("x must be of class \"conc\"")
  }    
  as_tibble(as.data.frame(x))
}

# Public functions applied to class ============================================

#' Print a concordance in KWIC format
#' 
#' Print a concordance in KWIC format.
#' 
#' @param x An object of class \code{conc}.
#' @param min_c_left,max_c_left Minimum and maximum size, expressed in number of
#'   characters, of the left co-text in the KWIC display.
#' @param min_c_match,max_c_match Minimum and maximum size, expressed in number of
#'   characters, of the match in the KWIC display.
#' @param min_c_right,max_c_right Minimum and maximum size, expressed in number of
#'   characters, of the right co-text in the KWIC display.
#' @param from Index of the first item of \code{x} to be displayed.
#' @param n Number of consecutive items in \code{x} to be displayed.
#' @param drop_tags Boolean. Should tags be hidden?
#'
#' @return Invisibly, \code{x}.
#' @export
#' @seealso \code{\link{print.conc}}
#'
#' @inherit print.conc examples
print_kwic <- function(x, 
                       min_c_left = NA,
                       max_c_left = NA,
                       min_c_match = NA,
                       max_c_match = NA,
                       min_c_right = NA,
                       max_c_right = NA,
                       from = 1,
                       n = 30,
                       drop_tags = TRUE) {
  # testing and processing argument 'x'
  if (! "conc" %in% class(x)) {
    stop("argument 'x' must be of the class 'conc'")
  }
  
  n_items <- nrow(x)
  if (length(x$left) == 0 ||
      length(x$match) == 0 ||
      length(x$right) == 0) {
    stop("x must have appropriate values for 'left', 'match', and 'right'")
  }
  
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
  from <- max(0, round(from))
  
  # adjusting 'n' to 'from'
  n <- max(0, min(n, n_items - from + 1))
  
  # making color styles
  mclm_light_gray <- crayon::make_style(rgb(0.8, 0.8, 0.8))
  
  if (n == 0) {
    return(invisible(x))
  } # the rest will only run if (n > 0)
  
  # print x
  idx <- from:(from + n - 1)
  idx_col   <- as.character(idx)
  left_col  <- as.character(x$left[idx])
  match_col <- as.character(x$match[idx])
  right_col <- as.character(x$right[idx])
  
  if (drop_tags) {
    left_col <- cleanup_spaces(drop_tags(left_col))
    match_col <- cleanup_spaces(drop_tags(match_col))
    right_col <- cleanup_spaces(drop_tags(right_col))
  }
  
  largest_idx <- max(nchar("idx"), nchar(idx_col), 0)
  largest_left <- max(nchar("left"), nchar(left_col), 0, na.rm = TRUE)
  largest_match <- max(nchar("match"), nchar(match_col), 0, na.rm = TRUE)
  largest_right <- max(nchar("right"), nchar(right_col), 0, na.rm = TRUE)  
  
  # calculate width for id_col, left_col, match_col, and right_col
  c_avail <- max(0, getOption("width") - 7) # 7 for spaces between columns
  # with some margin for scroll bar
  
  # -- id col
  c_idx_col <- largest_idx
  idx_col <- stringi::stri_pad_left(idx_col, c_idx_col) 
  c_avail <- max(0, c_avail - c_idx_col)
  
  # -- match col
  c_match_col <- trunc(c_avail * 0.3)
  if (! is.na(min_c_match)) {
    c_match_col <- max(0, min_c_match, c_match_col)
  }
  if (! is.na(max_c_match)) {
    c_match_col <- max(0, min(c_match_col, max_c_match))
  }
  c_match_col <- min(c_match_col, largest_match)
  match_col <- stringr::str_trunc(
    match_col, c_match_col, side = "center")
  match_col <- stringi::stri_pad_both(match_col, c_match_col)
  
  # -- right col
  c_avail <- max(0, c_avail - c_match_col)
  c_right_col <- trunc(c_avail * 0.5)
  if (! is.na(min_c_right)) {
    c_right_col <- max(0, min_c_right, c_right_col)
  }
  if (! is.na(max_c_right)) {
    c_right_col <- max(0, min(c_right_col, max_c_right))
  }
  c_right_col <- min(c_right_col, largest_right)
  right_col <- stringr::str_trunc(
    right_col, c_right_col, side = "right")
  right_col <- stringi::stri_pad_right(right_col, c_right_col)
  
  # -- left col
  c_left_col <- max(0, c_avail - c_right_col)
  if (! is.na(min_c_left)) {
    c_left_col <- max(0, min_c_left, c_left_col)
  }
  if (! is.na(max_c_left)) {
    c_left_col <- max(0, min(c_left_col, max_c_left))
  }
  left_col <- stringr::str_trunc(
    left_col, c_left_col, side = "left")
  left_col <- stringi::stri_pad_left(left_col, c_left_col)
  
  # -- print title
  cat(stringi::stri_pad_left(stringr::str_trunc("idx",
                                                c_idx_col,
                                                side = "center"),
                             c_idx_col))
  cat(" ")
  cat(stringi::stri_pad_left(stringr::str_trunc("left",
                                                c_left_col,
                                                side = "center"),
                             c_left_col))
  cat(mclm_light_gray("|"))
  cat(stringi::stri_pad_both(stringr::str_trunc("match",
                                                c_match_col,
                                                side = "center"),
                             c_match_col))    
  cat(mclm_light_gray("|"))
  cat(stringi::stri_pad_right(stringr::str_trunc("right",
                                                 c_right_col,
                                                 side = "center"),
                              c_right_col))    
  
  cat("\n")
  # --
  if (from > 1) cat("...\n")
  # --  
  for (i in seq_along(idx)) {
    cat(mclm_light_gray(idx_col[i]))
    cat(" ")
    cat(left_col[i])
    cat(mclm_light_gray("|"))
    cat(crayon::blue(match_col[i]))
    cat(mclm_light_gray("|"))
    cat(right_col[i])
    cat("\n")
  }
  
  # --
  if ((from + n - 1) < n_items) cat("...\n")
  
  invisible(x)
}

#' Read a concordance from a file
#' 
#' Reads concordance-based data frames that are written to file with the function
#' \code{\link{write_conc}}.
#'
#' @param file Name of the input file.
#' @param sep Field separator used in the input file.
#' @param file_encoding Encoding of the input file.
#' @param stringsAsFactors Boolean. Whether character data should automatically
#'   be converted to factors. It applies to all columns except for \code{"source"},
#'   \code{"left"}, \code{"match"} and \code{"right"}, which are never converted.
#' @param ... Additional arguments, not implemented.
#'
#' @return Object of class \code{conc}.
#' @seealso \code{\link{write_conc}}
#' @export
#'
#' @examples
#' \dontrun{
#' (d <- conc('A very small corpus.', '\\w+', as_text = TRUE))
#' write_conc(d, "example_data.tab")
#' (d2 <- read_conc("example_data.tab"))
#' }
read_conc <- function(file,
                      sep = "\t",
                      file_encoding = "UTF-8",
                      stringsAsFactors = FALSE,
                      ...) {
  # NOTE the previous implementation included some arguments that were not used
  # and for which documentation was included (which said "not implemented".)
  # - header: whether the first line in the input file contains column names.
  #           Always assumed to be TRUE.
  # - quote: whether data fields are enclosed in quotation marks (or what other
  #          character). Assumed to be "" - not enclosed.
  # - comment_char: whether there are commented lines? Simply not implemented.
  lines <- read_txt(file, file_encoding = file_encoding) 
  cols <- unlist(strsplit(lines[[1]], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
    col_name <- cols[i]
    d[[col_name]] <- unlist(lapply(cells, "[", i))
    if (col_name %in% c("source", "left", "match", "right")) {
      d[[col_name]] <- type.convert(d[[col_name]], as.is = TRUE)
    } else {
      d[[col_name]] <- type.convert(d[[col_name]],
                                    as.is = ! stringsAsFactors) 
    }   
  }
  class(d) <- c("conc", class(d))
  d
}

#' Write a concordance to file.
#' 
#' Write an object of class \code{conc} to a file.
#'
#' @param x Object of class \code{conc}.
#' @param file Path to output file.
#' @param sep Field separator for the columns in the output file.
#' @param file_encoding Encoding to be used in the output file.
#'
#' @return Invisibly, \code{x}.
#' @export
#' @seealso \code{\link{read_conc}}
#'
#' @inherit read_conc examples
write_conc <- function(x,
                       file = "",
                       sep = "\t",
                       file_encoding = "UTF-8") {
  if (nrow(x) > 0 && ncol(x) > 0) {
    lines <- as.character(x[[1]])
    if (ncol(x) > 1) {
      for (i in 2:ncol(x)) {
        lines <- paste(lines, as.character(x[[i]]), sep = sep)
      }
    }
    names <- paste(names(x), collapse = sep)
    x <- paste0(paste(append(names, lines), collapse = "\n"), "\n")
    con <- file(file, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)
  }
  invisible(x)
}

import_conc <- function(x,
                        file_encoding = "UTF-8",
                        source_type = c("corpuseye"),
                        ...) {
  if (! is.character(source_type)) {
    stop("the argument 'source_type' must be a character string")
  }
  source_type <- tolower(source_type)
  switch(source_type,
         corpuseye = import_conc_corpuseye(x, ...))
}  

merge_conc <- function(..., show_warnings = TRUE) {
  if (show_warnings) { 
    df <- dplyr::bind_rows(...)
  } else {
    suppressWarnings(dplyr::bind_rows(...)) 
  } 
  df$glob_id <- 1:nrow(df)
  df
}

# Private functions applied to class ===========================================

import_conc_corpuseye <- function(x, ...) {
  if (! is.character(x)) {
    stop("argument 'x' must be a character vector containing file names")
  } 
  corpdata <- character(0)
  for (in_file in x) {
    new_lines <- read_txt(in_file, encoding = "UTF-8") 
    corpdata <- append(corpdata, new_lines)
  }
  # IDEA could this be done with a pipe?
  corp <- paste(corpdata, collapse = " \n")
  corp <- gsub("([^\n]) \n([^\n])", "\\1 \\2", corp, perl = TRUE)
  corp <- gsub("\n \n", "\n", corp, perl = TRUE)
  corp <- gsub("\\*", "\t", corp, perl = TRUE)
  corp <- gsub("\\|", "", corp, perl = TRUE)
  corp <- unlist(strsplit(corp, "\n"))
  corp <- corp[grep("[^\\s]", corp, perl = TRUE)]
  corp <- paste("corpuseye", corp, sep = "\t")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", corp)
  # --  
  cols <- unlist(strsplit(lines[1], "\t"))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, "\t")
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
    col_name <- cols[i]
    d[[col_name]] <- unlist(lapply(cells, "[", i))
    d[[col_name]] <- type.convert(d[[col_name]], as.is = TRUE)
  }
  class(d) <- c("conc", class(d))
  d
}


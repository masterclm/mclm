# Create or coerce to class ====================================================
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
  }
  # --
  if (is.na(left) || left == "left") {
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
  }
  # --
  if (is.na(match) || match == "match") {
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
  }
  # --
  if (is.na(right) || right == "right") {
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

as.data.frame.conc <- function(x, ...) {
  if (!"conc" %in% class(x)) {
    stop("x must be of class \"conc\"")
  }    
  d <- x
  class(d) <- setdiff(class(d), "conc")
  d
}

as_tibble.conc <- function(x, ...) {
  if (!"conc" %in% class(x)) {
    stop("x must be of class \"conc\"")
  }    
  as_tibble(as.data.frame(x))
}

# Public functions applied to class ============================================

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
  # print x
  if (n > 0) {
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
  }
  invisible(x)
}

read_conc <- function(file,
                      header = TRUE,
                      sep = "\t",
                      quote = "",
                      comment_char = "",
                      file_encoding = "UTF-8",
                      stringsAsFactors = FALSE,
                      ...) {
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


# write a concordance
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


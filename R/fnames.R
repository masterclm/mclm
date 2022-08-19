# Create an fnames object ======================================================
#' Retrieve the names of files in a given path
#' 
#' Build an object of class `fnames`.
#'
#' @param path The location of the files to be listed.
#' @param re_pattern Optional regular expression. If present, then only the
#'   filenames that match it are retrieved (unless `invert = TRUE`, in which
#'   case those filenames are excluded). The match is done over the absolute
#'   path of the files.
#' @param recursive Boolean value. Should the subdirectories of `path` also be
#'   searched?
#' @param perl Boolean value. Whether `re_pattern` should be interpreted as a
#'   PERL flavour of regular expression.
#' @param invert Boolean value. If `TRUE`, filenames matching `re_pattern` are
#'   the only ones retrieved. If `FALSE`, filenames matching `re_pattern` are
#'   excluded.
#'
#' @return An object of class `fnames`, which is a special kind of character
#'   vector storing the absolute paths of the corpus files.
#'   It has additional attributes and methods such as:
#'   - base [`print()`][print.freqlist()], [as_data_frame()],
#'   [sort()] and [summary()] (which returns the number of items and of unique items),
#'   - [tibble::as_tibble()],
#'   - an interactive [explore()] method,
#'   - a function to get the number of items [n_fnames()],
#'   - subsetting methods such as [keep_types()], [keep_pos()], etc. including `[]`
#'   subsetting (see [brackets]), as well as the specific functions [keep_fnames()]
#'   and [drop_fnames()].
#'   
#'   Additional manipulation functions includes [fnames_merge()] to combine
#'   filenames collections and the [short_names()] family of functions to shorten
#'   the names.
#'   
#'   Objects of class `fnames` can be saved to file with [write_fnames()];
#'   these files can be read with [read_fnames()].
#'   
#'   It is possible to coerce a character vector into an `fnames` object with [as_fnames()].
#' @export
#' @name fnames
#'
#' @examples
#' \dontrun{
#' cwd_fnames <- get_fnames(recursive = FALSE)
#' }
#' cwd_fnames <- as_fnames(c("file1", "file2", "file3"))
#' cwd_fnames
#' print(cwd_fnames)
#' as_data_frame(cwd_fnames)
#' as_tibble(cwd_fnames)
#' 
#' sort(cwd_fnames)
#' 
#' summary(cwd_fnames)
get_fnames <- function(path = ".",
                  re_pattern = NULL,
                  recursive = TRUE,
                  perl = TRUE,
                  invert = FALSE) {
  x <- as_fnames(list.files(path = path, 
                           full.names = TRUE, 
                           recursive = recursive),
                 remove_duplicates = FALSE, # list.files() already did this
                 sort = FALSE)              # list.files() already did this    
  if (! is.null(re_pattern) && ! is.na(re_pattern[[1]])) {
    x <- keep_re(x, pattern = re_pattern, invert = invert, perl = perl)
  }
  x
}

#' Coerce object to 'fnames'
#' 
#' This function coerces a character vector into an object of class [`fnames`].
#'
#' @param x A character vector (or a [`freqlist`] object!)
#' @param remove_duplicates Boolean. Whether duplicates should be removed.
#' @param sort Boolean. Whether the output should be sorted.
#' @param ... Additional arguments.
#'
#' @return An object of class [`fnames`].
#' @export
#' 
#' @examples
#' as_fnames("path/to/my/corpus_file")
as_fnames <- function(x,
                      remove_duplicates = TRUE,
                      sort = TRUE,
                      ...) {
  result <- x
  if ("freqlist" %in% class(result)) {
    result <- type_names(result)
  }
  if (is.null(result)) {
    result <- vector(mode = "character", length = 0)
  } else if (!"character" %in% class(result)) {
    result <- as.character(result)
  }
  result <- result[!is.na(result)] # remove NAs
  if (remove_duplicates && (length(result) > 0)) {
    result <- names(table(result))
  }
  if (sort) {
    result <- stringr::str_sort(result)
  }
  class(result) <- c("fnames",
                     setdiff(class(result), c("types", "tokens")))
  result
}

# S3 methods from mclm =========================================================

#' @rdname explore
#' @exportS3Method explore fnames
explore.fnames <- function(x,
                           n = 20,
                           from = 1,
                           perl = TRUE,
                           use_clear = TRUE,
                           ...) {
  if (interactive()) {
    length_x <- n_fnames(x)                     # n items in x
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

## Subsetting ------------------------------------------------------------------

#' @rdname keep_pos
#' @exportS3Method drop_pos fnames
drop_pos.fnames <- function(x, pos, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_pos.fnames(x, pos, invert = TRUE, ...)
}

#' @rdname keep_pos
#' @exportS3Method keep_pos fnames
keep_pos.fnames <- function(x, pos, invert = FALSE, ...) {
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
    return(x)
  }
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
  subset_fnames(x, mtch)
}

#' @rdname keep_types
#' @exportS3Method drop_types fnames
drop_types.fnames <- function(x, types, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_types.fnames(x, types, invert = TRUE, ...)
}

#' @rdname keep_types
#' @exportS3Method keep_types fnames
keep_types.fnames <- function(x, types, invert = FALSE, ...) {
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
    return(x)
  }
  mtch <- !is.na(match(x, types)) # we avoid x_ranks[types] and x[types]
  if (invert) {
    mtch <- !mtch
  }
  
  subset_fnames(x, mtch)
}

#' @rdname keep_bool
#' @exportS3Method drop_bool fnames
drop_bool.fnames <- function(x, bool, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_bool.fnames(x, !bool, ...)
}

#' @rdname keep_bool
#' @exportS3Method keep_bool fnames
keep_bool.fnames <- function(x, bool, invert = FALSE, ...) {
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
    return(x)
  }
   
  subset_fnames(x, bool)
}

#' @rdname keep_re
#' @exportS3Method drop_re fnames
drop_re.fnames <- function(x, pattern, perl = TRUE, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_re.fnames(x, pattern, perl = perl, invert = TRUE, ...)
}

#' @rdname keep_re
#' @exportS3Method keep_re fnames
keep_re.fnames <- function(x, pattern, perl = TRUE, invert = FALSE, ...) {
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
    return(x)
  }
  sel <- grep(pattern[1], x, perl = perl[1], invert = invert[1])
  # create result
  if (length(sel) == 0) {
    result <- as_fnames(character(0))
  } else {
    result <- subset_fnames(x, sel)
  }
  # return result
  result
}

#' @rdname brackets
#' @exportS3Method `[` fnames
`[.fnames` <- function(x, i, invert = FALSE, ...) {
  if (missing(i) || is.null(i)) {
    return(x)
  }
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
  result
}

#' @rdname brackets
#' @exportS3Method `[<-` fnames
`[<-.fnames` <- function(x, i, invert = FALSE, value) {
  stop("subset assignment is not supported for 'fnames' objects")
}

# S3 methods from other packages ===============================================

#' @rdname as_data_frame
#' @exportS3Method as.data.frame fnames
as.data.frame.fnames <- function(x, ...) {
  class(x) <- "character"
  data.frame(filename = x, ...)
}

#' @exportS3Method tibble::as_tibble fnames
as_tibble.fnames <- function(x, ...) {
  tibble(filename = x, ...)
}

#' @exportS3Method sort fnames
sort.fnames <- function(x, decreasing = FALSE, ...) {
  as_fnames(sort(as_character(x),
                 decreasing = decreasing,
                 na.last = NA,
                 ...),
            remove_duplicates = FALSE, # not needed
            sort = FALSE)              # sort() already did this    
}

#' @rdname stubs
#' @exportS3Method plot fnames
plot.fnames <- function(x, ...) {
  warning("'fnames' objects have no plotting function; doing nothing")
  invisible(NULL)
}

#' @rdname mclm_print
#' @exportS3Method print fnames
print.fnames <- function(x,
                         n = 20, from = 1,
                         sort_order = c("none", "alpha"),
                         extra = NULL,
                         ...) {
  n_types <- length(x)
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
  # testing and processing argument 'sort_order'
  if (is.null(sort_order)  ||
      is.na(sort_order[1]) ||
      (!sort_order[1] %in% c("alpha", "none"))) {
    sort_order <- "none"
    warning("unknown sort_order value found; 'none' assumed")
  }  
  if (n > 0) {
    idx <- from:(from + n - 1)
    ord <- idx # applies when sort_order is 'none'
    if (sort_order[1] == "alpha") {
      ord <- order(x)[idx]
    }  
  }
  # testing argument 'extra'
  if (!is.null(extra) && !is.environment(extra)) {
    stop("incorrect use of the argument 'extra'")
  }  
  # printing 'x'
  cat(mclm_style_dim(paste0(
    "Filename collection of length ",
    n_types,
    "\n")))
  if (n == 0) {
    return(invisible(x))
    }
  types <- x[ord]
  format_idx <- format(c("", 
                         format(idx, scientify = FALSE, 
                                justify = "right")), 
                       justify = "right")    
  # we don't use format() [problems with unicode !]
  # nor do we use stringi::stri_pad_left [hickups with greek and Set.locale]
  nchar_types <- nchar(types)
  if (!is.null(extra$type_regex)) {
    types <- show_matches(types, extra$type_regex)
  }    
  format_types <- mclm_pad_left(
    c("filename", types),
    max(nchar("filename"), nchar_types),
    nchar_x = c(nchar("filename"), nchar_types))
  # -- print titles
  cat(format_idx[1], " ", sep = "")
  cat(format_types[1], "\n", sep = "")
  # -- print horizontal lines
  cat(paste0(rep_len(" ", nchar(format_idx[1])), collapse = ""),
      " ",
      paste0(rep_len("-", nchar(format_types[1])), collapse = ""),
      sep = "")
  cat("\n")
  # -- optionally print dots
  if (from > 1) cat(mclm_style_very_dim("...\n"))
  # -- print items  
  for (j in seq_along(idx)) {
    cat(mclm_style_very_dim(format_idx[j + 1]), " ", 
        format_types[j + 1], "\n", sep = "")
  }
  # -- optionally print dots
  if ((from + n - 1) < n_types) cat(mclm_style_very_dim("...\n"))
  invisible(x)
}
## Summary ---------------------------------------------------------------------

#' @exportS3Method summary fnames
summary.fnames <- function(object, ...) {
  result <- list(
    n_items = length(object),
    n_unique_items = length(table(object))
  )
  class(result) <- "summary.fnames"
  result
}

#' @exportS3Method print summary.fnames
print.summary.fnames <- function(x, ...) {
  cat("Filename collection of length ",
      x$n_items,
      "\n",
      sep = "")
  if (x$n_unique_items < x$n_items) {
    cat("[duplicates present and counted double]\n")
  }
  invisible(x)
}

#' @rdname stubs
#' @exportS3Method plot summary.fnames
plot.summary.fnames <- function(x, ...) {
  warning("'summary.fnames' objects have no plotting function; doing nothing")
  invisible(NULL)
}

# Public functions applied to class ============================================

#' Shorten filenames
#' 
#' Helper functions that make the paths to a file shorter.
#'
#' @param x An object of class [`fnames`] or a character vector.
#' @param ... Additional arguments.
#'
#' @return An object of the same class as `x`.
#' @name short_names
#'
#' @examples
#' cwd_fnames <- as_fnames(c("folder/file1.txt", "folder/file2.txt", "folder/file3.txt"))
#' drop_path(cwd_fnames)
#' drop_extension(cwd_fnames)
#' short_names(cwd_fnames) # same as drop_path(drop_extension(cwd_fnames))
NULL

#' @describeIn short_names Extract the base name of a path, removing the paths leading to it.
#' @export
drop_path <- function(x, ...) {
  gsub("^.*/([^/]*)$", "\\1", x, perl = TRUE)
}

#' @describeIn short_names Remove extension from a filename.
#' @export
drop_extension <- function(x, ...) {
  gsub("^(.*)[.][^.]*$", "\\1", x, perl = TRUE)
}

#' @describeIn short_names Remove both paths leading to a file and its extension.
#' @export
short_names <- function(x, ...) {
  drop_path(drop_extension(x, ...), ...)
}

#' Count number of items in an 'fnames' object
#' 
#' This function counts the number of items, duplicated or not, in an [`fnames`]
#' object. If there are duplicated items, it will return a warning.
#'
#' @param x Object of class [`fnames`].
#' @param ... Additional arguments.
#'
#' @return A number.
#' @export
#'
#' @examples
#' cwd_fnames <- as_fnames(c("folder/file1.txt", "folder/file2.txt", "folder/file3.txt"))
#' n_fnames(cwd_fnames)
n_fnames <- function(x, ...) {
  if (! "fnames" %in% class(x)) {
    stop("argument 'x' must be of the class 'fnames'")
  }
  without_duplicates <- length(table(x))
  with_duplicates <- length(x)
  if (without_duplicates < with_duplicates) {
    warning("duplicates detected and counted double in 'fnames' object")
  }
  with_duplicates
}  

#' Merge filenames collections
#' 
#' These functions merge two or more [`fnames`] objects into one larger [`fnames`]
#' object, removing duplicates (keeping only the first appearance) and only
#' resorting the items if `sort = TRUE`.
#'
#' @param x,y An object of class [`fnames`].
#' @param sort Boolean value. Should the items in the output be sorted?
#' @param ... Various objects of class [`fnames`] or a list of
#'   objects of class [`fnames`]. 
#'
#' @return An object of class [`fnames`].
#' @name merge_fnames
#' 
#' @examples
#' cwd_fnames <- as_fnames(c("file1.txt", "file2.txt"))
#' cwd_fnames2 <- as_fnames(c("dir1/file3.txt", "dir1/file4.txt"))
#' cwd_fnames3 <- as_fnames(c("dir2/file5.txt", "dir2/file6.txt"))
#' fnames_merge(cwd_fnames, cwd_fnames2)
#' fnames_merge_all(cwd_fnames, cwd_fnames2, cwd_fnames3)

#' @rdname merge_fnames
#' @export
fnames_merge <- function(x, y, sort = FALSE) {
  if ((!"fnames" %in% class(x)) || (!"fnames" %in% class(y))) {
    stop("both x and y must be of the class 'fnames'")
  }
  fnames_merge_two(x, y, sort = sort)
}  

#' @rdname merge_fnames
#' @export
fnames_merge_all <- function(..., sort = FALSE) {
  arg_list <- list(...)
  result_car <- NULL  # result for car of arg_list
  result_cdr <- NULL  # result for cdr of arg_list
  # -- processing car --
  if (length(arg_list) > 0) {
    car <- arg_list[[1]]
    if ("fnames" %in% class(car)) {
      result_car <- car
    } else if (is.list(car) && length(car) > 0) {
      result_car <- do.call("fnames_merge_all", car)
    } else {
      stop("Items must be of class 'fnames' or list of 'fnames'.")
    }
  }   
  # -- processing cdr --
  if (length(arg_list) > 1) {
    cdr <- arg_list[-1]
    result_cdr <- do.call("fnames_merge_all", cdr)
  }
  # -- merge results if needed ---
  result <- result_car
  if (is.null(result_car)) {
    result <- result_cdr
  } else if (!is.null(result_cdr)) {
    result <- fnames_merge_two(result_car, result_cdr)
  }
  # -- sort if needed --
  if (sort) {
    result <- as_fnames(result,
                        remove_duplicates = FALSE,
                        sort = TRUE)
  }
  # -- result --
  result
}

#' Filter collection of filenames by name
#' 
#' The functions build a subset of an object of class [`fnames`] based on a vector
#' of characters, either including them (with `keep_fnames(invert = FALSE)`) or
#' excluding them (with `keep_fnames(invert = FALSE)` or `drop_fnames()`).
#'
#' @param x An object of class [`fnames`], to be filtered.
#' @param y An object of class [`fnames`] or class [`types`] or a character vector.
#'   This is the filtering criterion.
#' @param invert Boolean value. If `TRUE`, the elements in `y` are excluded rather
#'   than kept (and `keep_fnames()` behaves like `drop_fnames()`)
#' @param ... Additional arguments.
#'
#' @return An object of class [`fnames`].
#' @export
#'
#' @examples
#' all_fnames <- as_fnames(c("file1", "file2", "file3",
#'                           "file4", "file5", "file6"))
#' 
#' unwanted_fnames <- as_fnames(c("file1", "file4"))
#' keep_fnames(all_fnames, unwanted_fnames, invert = TRUE)
#' drop_fnames(all_fnames, unwanted_fnames)
#' 
#' wanted_fnames <- as_fnames(c("file3", "file5"))
#' keep_fnames(all_fnames, wanted_fnames)
keep_fnames <- function(x, y, invert = FALSE, ...) {
  # -- test and process argument 'x'
  if (!"fnames" %in% class(x)) {
    stop("x must be of class 'fnames'")
  }
  # -- test and process argument 'types'
  types <- as.character(y) # turns NULL into character(0)
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
    return(x)
  }
  mtch <- !is.na(match(x, types)) # we avoid x_ranks[types] and x[types]
  if (invert) {
    mtch <- !mtch
  }
  
  subset_fnames(x, mtch)
}

#' @rdname keep_fnames
#' @export
drop_fnames <- function(x, y, ...) {
  dot_args <- names(list(...))
  if ("invert" %in% dot_args) {
    stop("argument 'invert' is not supported")
  }
  keep_fnames(x, y, invert = TRUE, ...)
}

#' Read a colelction of filenames from a text file
#' 
#' This function reads an object of class [`fnames`] from a text file, which is
#' assumed to contain one filename on each line.
#'
#' @param file Path to input file.
#' @param sep Character vector of length 1 or `NA`. If it is a character, it
#'   indicates a separator between input files, in addition to the new line.
#' @param file_encoding Encoding used in the input file.
#' @param trim_fnames Boolean. Should leading and trailing whitespace be stripped
#'   from the filenames?
#' @param ... Additional arguments (not implemented).
#'
#' @return An object of class [`fnames`].
#' @export
#' @family reading functions
#' @seealso [write_fnames()]
#'
#' @examples
#' \dontrun{
#' cwd_fnames <- get_fnames(recursive = FALSE)
#' write_fnames(cwd_fnames, "file_with_filenames.txt")
#' cwd_fnames_2 <- read_fnames("file_with_filenames.txt")
#' }
read_fnames <- function(file,
                        sep = NA,
                        file_encoding = "UTF-8",
                        trim_fnames = FALSE,
                        ...) {
  result <- readr::read_lines(file,
                              locale = readr::locale(encoding = file_encoding))
  if (!is.na(sep) && is.character(sep)  && (length(sep) > 0)) {
    result <- unlist(stringr::str_split(result, sep[1]))
  }
  if (trim_fnames) {
    result <- stringr::str_trim(result) 
  }
  # QUESTION config file is not read?
  # to consider: not run following line if config file says
  #              txt_comment_char is "" and not "#"
  result <- gsub('#.*$', '', result, perl = TRUE) 
  result <- result[nchar(result) > 0]
  result <- restore_unicode(result) 
  class(result) <- c("fnames", setdiff(class(result), c("tokens", "types")))  
  result
}

#' Write a collection of filenames to a text file
#' 
#' This function writes an object of class [`fnames`] to a text file. Each filename
#' is written in a separate line. The file encoding is always `"UTF-8"`.
#' In addition, it can store metadata in an additional configuration file.
#'
#' @param x Object of class [`fnames`].
#' @param file Path to output file.
#' @param ... Additional arguments (not implemented).
#'
#' @return Invisibly, `x`.
#' @export
#' @family writing functions
#' @seealso [read_fnames()]
#'
#' @inherit read_fnames examples
write_fnames <- function(x,
                         file,
                         ...) {
  # TODO reinstate make_config file and use with read_fnames
  # TODO add encoding options?
  if (! "fnames" %in% class(x)) {
    stop("argument 'x' must be of the class 'fnames'")
  }
  # hide any real '#' that is actually part of a type
  x <- gsub('#', '<U+0023>', x, perl = TRUE) 
  readr::write_lines(x, file)
  invisible(x)
}

# Private functions applied to class ===========================================

#' Merge two 'fnames' objects
#'
#' @param x,y Objects of class [fnames()]
#' @param sort Boolean. Whether to sort the output
#'
#' @return An object of class [fnames()].
#' @noRd
fnames_merge_two <- function(x, y, sort = FALSE) {
  as_fnames(dplyr::union(x, y),
            remove_duplicates = FALSE, # done by union
            sort = sort)
}

#' Subset an 'fnames' object
#'
#' @param x Object of class [`fnames`].
#' @param sel Numeric vector with positions or boolean vector for subsetting.
#'
#' @return Object of class [`fnames`].
#' @noRd
subset_fnames <- function(x, sel) {
  result <- as.character(x)[sel]
  class(result) <- c("fnames",
                     setdiff(class(x),
                             c("tokens", "types")))
  result
}

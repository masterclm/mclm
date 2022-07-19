# Constructor ==================================================================

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

as_re <- function(x, perl = TRUE, ...) {
  re(x, perl = perl, ...)
}

# not a big fan of the following notation, but supporting it anyway
# IDEA remove?
as.re <- function(x, perl = TRUE, ...) {
  re(x, perl = perl, ...)
}

# Public functions applied to the class ========================================

perl_flavor <- function(x) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$perl
}

`perl_flavor<-` <- function(x, value) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$perl <- value
  x
}

# S3 Methods from other packages ===============================================
print.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  cat("Regular expression (perl = ", x$perl, ")\n", sep = "")
  cat("------------------\n")
  cat(x$x, "\n")
}

plot.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  warning("there is no plot method for 're' objects; doing nothing")
  invisible(NULL)
}

as.character.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$x
}

as_character.re <- function(x, ...) {
  if (!"re" %in% class(x)) {
    stop("x must be an object of the class 're'")
  }
  x$x
}

## Summary ---------------------------------------------------------------------
summary.re <- function(object, ...) {
  if (!"re" %in% class(object)) {
    stop("object must be an object of the class 're'")
  }
  result <- object
  class(result) <- c("summary.re", "re")
  result
}

print.summary.re <- function(x, ...) {
  if (!"summary.re" %in% class(x)) {
    stop("x must be an object of the class 'summary.re'")
  }
  cat("Regular expression (perl = ", x$perl, ")\n", sep = "")
  cat("------------------\n")
  cat(x$x, "\n")
}

plot.summary.re <- function(x, ...) {
  if (!"summary.re" %in% class(x)) {
    stop("x must be an object of the class 'summary.re'")
  }
  warning("there is no plot method for 'summary.re' objects; doing nothing")
  invisible(NULL)
}

# Public regex convenience functions ===========================================

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
  if (is.null(requested_group) || requested_group == 0) {
    result <- Map(function(x, m) h_regmatches(x, m)[[1]], 
                  x, m)
  } else if (length(m) > 0 &&
             requested_group >= 1 &&
             requested_group <= length(attr(m[[1]][[1]], 'capture.start')[1,])) {
    # in following line, changed m to m[[1]] on 2020-11-11
    # but switched back to m on 2021-07-29
    result <- Map(function(x, m) h_regmatchesgroup(x, m,
                                                   group = requested_group)[[1]], 
                  x, m)
  } else {
    result <- rep(NA, length(x))
  }
  names(result) <- NULL
  result <- unlist(lapply(result, h_get_first))
  if (drop_NA) {
    result <- result[!is.na(result)]
  }
  result
}

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
  if (is.null(requested_group) || requested_group == 0) {
    result <- Map(function(x, m) h_regmatches(x, m)[[1]], 
                  x, m)
  } else if (length(m) > 0 &&
             requested_group >= 1 &&
             requested_group <= length(attr(m[[1]][[1]], 'capture.start')[1,])) {
    # in following line, changed m to m[[1]] on 2020-11-11
    # but switched back to m on 2021-07-29
    result <- Map(function(x, m) h_regmatchesgroup(x, m,
                                                   group = requested_group)[[1]], 
                  x, m)
  } else {
    result <- rep(NA, length(x))
  }
  names(result) <- NULL
  result <- unlist(lapply(result, h_get_last))
  if (drop_NA) {
    result <- result[!is.na(result)]
  }
  result
}

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
  if (is.null(requested_group) || requested_group == 0) {
    result <- Map(function(x, m) h_regmatches(x, m)[[1]], 
                  x, m)
  } else if (length(m) > 0 &&
             requested_group >= 1 &&
             requested_group <= length(attr(m[[1]][[1]], 'capture.start')[1,])) {
    # in following line, changed m to m[[1]] on 2020-11-11
    # but switched back to m on 2021-07-29
    result <- Map(function(x, m) h_regmatchesgroup(x, m,
                                                   group = requested_group)[[1]], 
                  x, m)
  } else {
    result <- rep(NA, length(x))
  }
  names(result) <- NULL
  if (unlist) { result <- unlist(result) }
  result
}

re_has_matches <- function(x, pattern, 
                           ignore.case = FALSE, perl = TRUE,
                           fixed = FALSE, useBytes = FALSE, 
                           ...) {
  grepl(pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
}

re_which <- function(x, pattern, 
                     ignore.case = FALSE, perl = TRUE,
                     fixed = FALSE, useBytes = FALSE, 
                     ...) {
  grepl(pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes)
}

re_replace_first <- function(x, pattern, replacement,
                             ignore.case = FALSE, perl = TRUE,
                             fixed = FALSE, useBytes = FALSE, 
                             ...) {
  sub(pattern, replacement, x, ignore.case = ignore.case, 
      perl = perl, fixed = fixed, useBytes = useBytes)
}

re_replace_all <- function(x, pattern, replacement,
                           ignore.case = FALSE, perl = TRUE,
                           fixed = FALSE, useBytes = FALSE, ...) {
  gsub(pattern, replacement, x, ignore.case = ignore.case, 
       perl = perl, fixed = fixed, useBytes = useBytes)
}

# Private helper functions =====================================================

# regcapturedmatches.R
# https://gist.github.com/GegznaV/57b4ff13e6d7a8a8344e
# https://gist.github.com/MrFlick/10413321

h_regmatches <- function(x, m, invert = FALSE) {
  if (length(x) != length(m)) 
    stop(gettextf("%s and %s must have the same length", 
                  sQuote("x"), sQuote("m")), domain = NA)
  ili <- is.list(m)
  useBytes <- if (ili) 
    any(unlist(lapply(m, attr, "useBytes")))
  else any(attr(m, "useBytes"))
  if (useBytes) {
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | (asc != x)
    if (any(ind)) 
      Encoding(x[ind]) <- "bytes"
  }
  if (!ili && !invert) {
    so <- m[ind <- (!is.na(m) & (m > -1L))]
    eo <- so + attr(m, "match.length")[ind] - 1L
    return(substring(x[ind], so, eo))
  }
  y <- if (invert) {
    Map(function(u, so, ml) {
      if ((n <- length(so)) == 1L) {
        if (is.na(so)) 
          return(character())
        else if (so == -1L) 
          return(u)
      }
      beg <- if (n > 1L) {
        eo <- so + ml - 1L
        if (any(eo[-n] >= so[-1L])) 
          stop(gettextf("need non-overlapping matches for %s", 
                        sQuote("invert = TRUE")), domain = NA)
        c(1L, eo + 1L)
      }
      else {
        c(1L, so + ml)
      }
      end <- c(so - 1L, nchar(u))
      substring(u, beg, end)
    }, x, m, if (ili) 
      lapply(m, attr, "match.length")
    else attr(m, "match.length"), USE.NAMES = FALSE)
  }
  else {
    Map(function(u, so, ml) {
      if (length(so) == 1L) {
        if (is.na(so) || (so == -1L)) 
          return(character())
      }
      substring(u, so, so + ml - 1L)
    }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
  }
  names(y) <- names(x)
  y
}

h_regmatchesgroup <- function(x, m, invert = FALSE, group = 1) {
  if (length(x) != length(m)) 
    stop(gettextf("%s and %s must have the same length", 
                  sQuote("x"), sQuote("m")), domain = NA)
  ili <- is.list(m)
  useBytes <- if (ili) 
    any(unlist(lapply(m, attr, "useBytes")))
  else any(attr(m, "useBytes"))
  if (useBytes) {
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | (asc != x)
    if (any(ind)) 
      Encoding(x[ind]) <- "bytes"
  }
  if (!ili && !invert) {
    ind <- (!is.na(m) & (m > -1L))
    so <- attr(m, "capture.start")[ind][group]      
    eo <- so + attr(m, "capture.length")[ind][group] - 1L
    return(substring(x[ind], so, eo))
  }
  y <- if (invert) {
    Map(function(u, so, ml) {
      so <- if (nrow(so) >= group) so[,group] else rep(NA,ncol(so))
      ml <- if (nrow(ml) >= group) ml[,group] else rep(NA,ncol(ml))
      if ((n <- length(so)) == 1L) {
        if (is.na(so)) 
          return(character())
        else if (so == -1L) 
          return(u)
      }
      beg <- if (n > 1L) {
        eo <- so + ml - 1L
        if (any(eo[-n] >= so[-1L])) 
          stop(gettextf("need non-overlapping matches for %s", 
                        sQuote("invert = TRUE")), domain = NA)
        c(1L, eo + 1L)
      }
      else {
        c(1L, so + ml)
      }
      end <- c(so - 1L, nchar(u))
      substring(u, beg, end)
    }, x,
    if (ili) 
      lapply(m, attr, "capture.start")
    else
      attr(m, "capture.start"),
    if (ili) 
      lapply(m, attr, "capture.length")
    else
      attr(m, "capture.length"),
    USE.NAMES = FALSE)
  }
  else {
    Map(function(u, so, ml, group) {
      so <- if (nrow(so) >= group) so[,group] else rep(NA,ncol(so))
      ml <- if (nrow(ml) >= group) ml[,group] else rep(NA,ncol(ml))
      if (length(so) == 1L) {
        if (is.na(so) || (so == -1L)) 
          return(character())
      }
      substring(u, so, so + ml - 1L)
    }, x,
    lapply(m, attr, "capture.start"),
    lapply(m, attr, "capture.length"),
    group,
    USE.NAMES = FALSE)
  }
  names(y) <- names(x) 
  y
}

h_get_first <- function(x) {
  result <- NA
  if (!is.null(x) && length(x) > 0) {
    result <- x[[1]]
  }
  result
}

h_get_last <- function(x) {
  result <- NA
  if (!is.null(x) && length(x) > 0) {
    result <- x[[length(x)]]
  }
  result
}

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


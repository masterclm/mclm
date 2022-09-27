#' Read a text file into a character vector
#' 
#' This function reads a text file and returns a character vector containing
#' the lines in the text file.
#'
#' @param file Name of the input file.
#' @param file_encoding Encoding of the input file.
#' @param line_glue A character vector or `NA`. If `NA`, the output is a character
#' vector in which each input line is a separate item, as in [readr::read_lines()].
#' Otherwise, the output is a character vector of length 1 in which all input lines
#' are concatenated, using the value of `line_glue[1]` as line separator and as
#' end-of-last-line marker.
#' @param ... Additional arguments (not implemented).
#'
#' @return A character vector.
#' @family reading functions
#' @seealso [write_txt()]
#' @export
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' x <- "This is
#' a small
#' text."
#' 
#' # write the text to a text file
#' write_txt(x, "example-text-file.txt")
#' # read a text from file
#' y <- read_txt("example-text-file.txt")
#' y
#' \dontshow{setwd(.old_wd)}
read_txt <- function(file,
                     file_encoding = "UTF-8",
                     line_glue = NA,
                     ...) {
  lines <- readr::read_lines(
    file,
    locale = readr::locale(encoding = file_encoding))    
  if (! is.na(line_glue)) {
    lines <- paste0(paste(lines, collapse = line_glue), line_glue)
  }
  lines
}

#' Write a character vector to a text file
#' 
#' This function writes a character vector to a text file. By default, each
#' item in the character vector becomes a line in the text file.
#'
#' @param x A character vector.
#' @param file Name of the output file.
#' @param line_glue Character string to be used as end-of-line marker on disk
#'   or `NA` for no end-of-line marker (so that `x` becomes a single line).
#'
#' @return Invisibly, `x`.
#' @export
#' @family writing functions
#' @seealso [read_txt()]
#'
#' @inherit read_txt examples
write_txt <- function(x,
                      file = "",
                      line_glue = "\n") {
  # TODO add encoding options
  if (! is.character(x) || length(x) == 0) {
    stop("argument 'x' must be a character vector of at least length one")
  }
  if (! is.character(line_glue) || length(line_glue) != 1) {
    stop("argument 'line_glue' must be a character vector of length one")
  }
  x <- paste0(paste(x, collapse = line_glue), line_glue)
  write_txt_utf8(x, file = file)
  invisible(x)
}

#' Write text to file with UTF encoding??
#' @noRd
write_txt_utf8 <- function(x,
                           file = "") {
  if (! is.character(x) || length(x) != 1) {
    stop("argument 'x' must be a character vector of length one")
  } 
  con <- file(file, "wb")
  writeBin(charToRaw(x), con, endian = "little")
  close(con)
  invisible(x)
}

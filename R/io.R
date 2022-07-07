# --
# reads a text file into a character vector
# - if line_glue is NA, each input line is a separate item in
#   the character vector
# - if line_glue is "\n", then the character vector contains
#   a single item, in which all input lines are concatenated,
#   using "\n" as line terminator.
# --
read_txt_old <- function(file,
                     file_encoding = "UTF-8",
                     line_glue = NA,
                     ...) {
  con <- file(file, encoding = file_encoding)
  lines <- readLines(con, warn = FALSE)
  close(con)
  if (! is.na(line_glue)) {
    lines <- paste0(paste(lines, collapse = line_glue), line_glue)
  }
  lines
}

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

write_txt <- function(x,
                      file = "",
                      file_encoding = "UTF-8",
                      line_glue = "\n") {
  # --
  # writes a character vector to a text file, using line_glue
  # as line terminator.
  # - if line_glue is NA, x is assumed to be a single line
  #   that is written to file as is 
  # --
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

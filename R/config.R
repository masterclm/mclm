#' Generate filename for a config file
#'
#' @param filename Character string
#'
#' @return Character string
#' @noRd
config_filename <- function(filename) {
  ret_val <- gsub("(?xi)  [.] [^.]+ $", "", filename, perl = TRUE)
  ret_val <- paste0(ret_val, ".yaml")
  ret_val
}

#' Read configuration file
#'
#' @param filename Character string.
#'
#' @return Contents of configuration file
#' @noRd
read_config <- function(filename) {
  ret_val <- NULL
  c_fname <- config_filename(filename)
  if (file.exists(c_fname)) {
    c_lines <- readr::read_lines(c_fname)
    ret_val <- yaml::yaml.load(paste0(c_lines, collapse = "\n"))
  }
  ret_val
}

# x: a list with config features and values
# filename: filename of either the corresponding data set (a csv file)
#                       or the actual config file (a yaml file)
#' Write configuration file
#'
#' @param x  List of config features and values
#' @param filename Either the corresponding data set (a csv file) or the actual
#'   config file (a yaml file).
#'
#' @return Invisibly, \code{x}.
#' @noRd
write_config <- function(x, filename) {
  c_fname <- config_filename(filename)
  c_lines <- yaml::as.yaml(x)
  readr::write_lines(c_lines, c_fname)
  invisible(x)
}

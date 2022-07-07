# ----------------------------------------------------------------------------
# implementation of the use of YAML-based config files in mclm
# ----------------------------------------------------------------------------
# namespace dependencies:
#   readr::read_lines()
#   readr::write_lines()
#   yaml::yaml.load()
#   yaml::as.yaml()
# ----------------------------------------------------------------------------

# filename: filename of either the corresponding data set (a csv file)
#                      or the actual config file (a yaml file)
config_filename <- function(filename) {
  ret_val <- gsub("(?xi)  [.] [^.]+ $", "", filename, perl = TRUE)
  ret_val <- paste0(ret_val, ".yaml")
  ret_val
}

# filename: filename of either the corresponding data set (a csv file)
#                       or the actual config file (a yaml file)
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
write_config <- function(x, filename) {
  c_fname <- config_filename(filename)
  c_lines <- yaml::as.yaml(x)
  readr::write_lines(c_lines, c_fname)
  invisible(x)
}





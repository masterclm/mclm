
#
#    name: mclm.R  [short for: mastering corpus linguistics methods]
#    purpose: library of simple R functions in support of corpus linguistics
#    author: Dirk Speelman 
#    version: 2017-10-16
#

.onUnload <- function(libpath) {
  library.dynam.unload("mclm", libpath)
}


# ---
#  FROM CONCORDANCES TO DATASETS
#

read_conc_antconc <- function(file,
                              version = c("3.4.3", "3.2.4"),
                              file_encoding = "") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: preferably YES (which
#           is not the default, but the default value NO is also OK)
# - Also, you must make sure to specify the correct file encoding.
  # --
  lines <- read_txt(file, file_encoding = file_encoding)
  # --
  d <- NA
  if (length(lines > 0)) {
    if (version[1] == "3.2.4") {
        # TAB is assumed to surround hit (but this is not cheched)
        cells <- strsplit(lines, split = "\t", fixed = TRUE)
        d <- data.frame(x = 1:length(lines)); d$x <- NULL
        d$id <- unlist(lapply(cells, "[", 1))
        d$left <- unlist(lapply(cells, "[", 2))
        d$match <- unlist(lapply(cells, "[", 3))
        d$right <- unlist(lapply(cells, "[", 4))
        d$source <- unlist(lapply(cells, "[", 5))   
    } else if (version[1] == "3.4.3") {
        # tab is preferred to surround hit (but may not)
        cells <- strsplit(lines, split = "\t", fixed = TRUE)
        d <- data.frame(x = 1:length(lines)); d$x <- NULL
        d$id <- unlist(lapply(cells, "[", 1))
        d$left <- unlist(lapply(cells, "[", 2))
        col3 <- unlist(lapply(cells, "[", 3))
        max_n_col3 <- max(nchar(col3))
        if (max_n_col3 == 0) {
          # there are TABS around the hits in KWIC display
          d$match <- unlist(lapply(cells, "[", 4))
          d$right <- unlist(lapply(cells, "[", 5))
          d$source <- unlist(lapply(cells, "[", 6))
        } else {
          # there are no TABS around the hits in KWIC display
          # we can only guess what are the boudaries of the hit
          d$match <- NA
          d$right <- unlist(lapply(cells, "[", 3))
          d$match <- gsub("^(\\s*[^ ]*).*$", "\\1", d$right, perl = TRUE)
          d$right <- substr(d$eight, nchar(d$match) + 1, nchar(d$right))
          d$source <- unlist(lapply(cells, "[", 4))       
        }
    }
  }
  d
}

conc_to_dataset_antconc <- function(x,
                                    outfile,
                                    version=c("3.4.3", "3.2.4"),
                                    file_encoding="") {
  if (length(file_encoding) < length(x)) {
    file_encoding <- rep(file_encoding, length = length(x))
  }
  lines <- vector()
  for (i in 1:length(x)) {
      d <- read_conc_antconc(x[i],
                             version = version, 
                             file_encoding = file_encoding[i])
      if (! is.null(dim(d))) {
        lines <- append(lines,
                        paste(as.character(d$id),
                             as.character(d$source),
                              as.character(d$left),
                              as.character(d$match),
                              as.character(d$right), sep = "\t"))
      }
  }
  lines <- paste(1:length(lines),"\t", lines, sep = "")
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", lines)
  y <- paste0(paste(lines, collapse = "\n"), "\n")
  con <- file(outfile, "wb")
  writeBin(charToRaw(y), con, endian = "little")
  close(con)
  invisible(x)
}

conc_to_dataset_corpuseye <- function(x, outfile) {
# ---
# - infiles are one or several files to which the concordance search
#   results from the corpuseye website (http://corp.hum.sdu.dk/)
#   were exported (by clicking on export all and then saving as *.txt)
# - such files always have encoding UTF-8
# - we also save the results with encoding UTF-8
# ---
  corpdata <- vector()
  for (infile in x) {
    lines <- read_txt(file)
    corpdata <- append(corpdata, lines)
  }
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
  # some more appending needs to be done here
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", corp)
  y <- paste0(paste(lines, collapse = "\n"), "\n")
  con <- file(outfile, "wb")
  writeBin(charToRaw(y), con, endian = "little")
  close(con)
  invisible(x)
}

conc_to_dataset_bibleworks <- function(x, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in seq_along(x)) {
    data_lines <- read_txt(x[i])
    data_lines <- data_lines[nchar(data_lines) > 0]
    id_lines <- seq_along(data_lines)
    metadata_lines <- read_txt(metafiles[i])
    metadata_lines <- metadata_lines[nchar(metadata_lines) > 0]
    source_lines <- rep(x[i], length(data_lines))
    id <- append(id, id_lines)
    data <- append(data, data_lines)
    metadata <- append(metadata, metadata_lines)
    source <- append(source, source_lines)
  }
  d <- data.frame(glob_id = seq_along(data),
                  id = id,
                  source = source,
                  data = data,
                  metadata = metadata)
  d <- d[-nrow(d), ] # in a future update, this should be done more elegantly
  write_dataset(d, outfile)
  invisible(x)
}

# read a data frame (simpler, but potentially more robust than read.table)
# (header, quote and comment_char have not been implemented yet)
read_dataset <- function(file,
                         header = TRUE,
                         sep = "\t",
                         quote = "",
                         comment_char = "",
                         file_encoding="UTF-8",
                         stringsAsFactors = default.stringsAsFactors(),
                         ...) {
  lines <- read_txt(file, file_encoding = file_encoding)
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
      col_name <- cols[i]
      d[[col_name]] <- unlist(lapply(cells, "[", i))
      d[[col_name]] <- type.convert(d[[col_name]],
                                    as.is = ! stringsAsFactors)
  }
  d
}

write_dataset <- function(x,
                          file = "",
                          sep = "\t",
                          file_encoding = "UTF-8") {
  if (nrow(x) > 0) {
    lines <- as.character(x[, 1])
    for (i in 2:ncol(x)) {
      lines <- paste(lines, as.character(x[, i]), sep = sep)
    }
    names <- paste(names(x), collapse = sep)
    x <- paste0(paste(append(names, lines), collapse = "\n"), "\n")
    con <- file(file, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)
  }
  invisible(x)
}

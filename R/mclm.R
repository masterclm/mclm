# -----------------------------------------------------------------------------
#
#    name: mclm.R  [short for: mastering corpus linguistics methods]
#    purpose: library of simple R functions in support of corpus linguistics
#    author: Dirk Speelman 
#    version: 2017-10-16
#
# -----------------------------------------------------------------------------


.onUnload <- function(libpath) {
  library.dynam.unload("mclm", libpath)
}


# ============================================================================
# deprecated generics
# ============================================================================

zoom_re <- function(x, pattern, perl = TRUE) UseMethod("zoom_re")

zoom_re.default <- function(x, pattern, perl = TRUE) x

details <- function(x, y) UseMethod("details")

details.default <- function(x, y) NULL

unzoom <- function(x) UseMethod("unzoom")

unzoom.default <- function(x) x





# ---
#  READ RAW CORPUS FILE
#

read.raw.corpfile.tokens <- function(file, fileEncoding="") {
# -- helper function of read.raw.corpus.freqinfo()
  # --
  con <- file(file, encoding = fileEncoding)
  lines <- readLines(con, warn=FALSE)
  close(con)    
  # --
  #lines <- scan(file, what="char", sep="\n", 
  #              fileEncoding=fileEncoding, blank.lines.skip = FALSE,
  #              quiet=TRUE)
  long.line <- paste(lines, collapse="")
  return(strsplit(long.line, "\\s+", perl=TRUE)[[1]])
}

make.freqlist <- function(tokens) {
# -- helper function of read.raw.corpus.freqinfo()
  return(table(as.factor(tokens)))
}

merge.freqlists <- function(flist1, flist2) {
# -- helper function of read.raw.corpus.freqinfo()
  names <- union(names(flist1), names(flist2))
  flist1 <- flist1[names]; names(flist1) <- names; flist1[is.na(flist1)] <- 0
  flist2 <- flist2[names]; names(flist2) <- names; flist2[is.na(flist2)] <- 0
  return(flist1 + flist2)
}

merge_freqlists <- function(x, y) {
  names <- union(names(x), names(y))
  x <- x[names]
  names(x) <- names
  x[is.na(x)] <- 0
  y <- y[names]
  names(y) <- names
  y[is.na(y)] <- 0
  x + y
}


# ---
#  FREQUENCY LISTS, KEYWORDS AND COLLOCATION RETRIEVAL FUNCTIONS
#

raw.corpus.freqlist <- function(fnames, 
                                fileEncoding="", 
                                drop.tokens=NULL) {
  freqlist <- numeric(0)
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     newtokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       newtokens <- newtokens[- grep(drop.tokens, newtokens, perl=TRUE)]
     }
     tokens    <- append(tokens, newtokens)
     cat("."); utils::flush.console()
     if ((i %% 20) == 0) {
       freqlist <- merge.freqlists(freqlist, make.freqlist(tokens))
       tokens <- character(0)
     }
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }
  }
  freqlist <- merge.freqlists(freqlist, make.freqlist(tokens))
  tokens <- numeric(0)
  return(freqlist)
}

raw.corpus.surface.cooccur <- function(fnames, 
                                       freqlist,
                                       L=3, 
                                       R=3,
                                       boundary.marker="<boundary />",
                                       fileEncoding="", 
                                       drop.tokens=NULL) {
  # ---
  itemlen <- length(freqlist)
  itempos <- 1:itemlen; names(itempos) <- names(freqlist)
  # ---
  pair.names <- outer(names(freqlist), names(freqlist), paste, sep=" | ")
  dim(pair.names) <- NULL
  pair.freqs <- rep(0, length(pair.names))
  names(pair.freqs) <- pair.names
  # ---
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     tokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       tokens <- tokens[- grep(drop.tokens, tokens, perl=TRUE)]
     }
     cat("."); utils::flush.console()
     # ---
     for (j in 1:length(tokens)) { # loop over collocates
       colloc <- tokens[j]
       if (! is.na(freqlist[colloc])) {
         nodes <- character(0)
         # --- look for nodes to left of collocate ---
         k <- 1
         while ((k <= R) && ((j - k) > 0)) {  # R from persp. of colloc is L from persp. of node
           node <- tokens[j - k]
           if (is.na(node)) {
             k <- R+1
           } else if (node == boundary.marker) {
             k <- R+1
           } else {
             if (! is.na(freqlist[node])) {
               nodes <- append(nodes, node)             
             }
             k <- k+1
           }
         }  # end of while (k <= R) {  }
         # --- look for nodes to right of collocate ---
         k <- 1
         while (k <= L) {  # L from persp. of colloc is R from persp. of node
           node <- tokens[j + k]
           if (is.na(node)) {
             k <- L+1
           } else if (node == boundary.marker) {
             k <- L+1
           } else {
             if (! is.na(freqlist[node])) {
               nodes <- append(nodes, node)
             }
             k <- k+1
           }
         }  # end of while (k <= L) {  }
         if (length(nodes) > 0) {
           for (node in levels(as.factor(nodes))) {
               pos <- itempos[colloc] + (itempos[node]-1)*itemlen
               pair.freqs[pos] = pair.freqs[pos] + 1
           }
         }
       }  # end of: if (! is.na(freqlist[colloc]))  {  }
     }  # end of: for (j in 1:length(tokens)) {  }
     # ---
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }  # end of: if (((i %% 60) == 0) | (i == length(fnames))) {  }
  }  # end of: for (i in 1:length(fnames)) { }
  return(pair.freqs)
}


# testen: hoe verkrijgen dat leestekens niet meegeteld worden: drop.token ?

raw.getsurfcooc.node <- function(
          files, 
          re.node,
          L=3, 
          R=3,
          re.boundary="<boundary />",
          re.split="[.'!?;;,\\s]+",
          re.ok.line=".*",
          re.drop.token=NA,
          to.lower=TRUE,
          perl=TRUE,
          blocksize=300,
          verbose=FALSE,
          dot.blocksize=10,
          fileEncoding="latin1") {
  first.pt <- proc.time(); new.pt <- first.pt
  retval <- list()
  globfreqlist1 <- vector()
  globfreqlist2 <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = fileEncoding)
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding=fileEncoding, 
      #                 blank.lines.skip = FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- unlist(strsplit(newlines, re.split, perl=perl))
      if (to.lower) { newtokens <- tolower(newtokens) }
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    match.pos <- grep(re.node, blocktokens, perl=perl)
    bound.pos <- vector()
    if (! is.na(re.boundary)) {
      bound.pos <- grep(re.boundary, blocktokens, perl=perl)
    }
    target.pos <- vector()
    new.pos <- match.pos
    if (L > 0) {
      for (k in 1:L) {
        new.pos <- new.pos - 1
        new.pos <- setdiff(new.pos, bound.pos)
        target.pos <- union(target.pos, new.pos)
      }
    }
    new.pos <- match.pos
    if (R > 0) {
      for (k in 1:R) {
        new.pos <- new.pos + 1
        new.pos <- setdiff(new.pos, bound.pos)
        target.pos <- union(target.pos, new.pos)
      }
    }
    target.pos <- target.pos[target.pos > 0]
    target.pos <- target.pos[target.pos <= length(blocktokens)]
    target.pos <- setdiff(target.pos, match.pos)
    
    ref.pos <- setdiff(1:length(blocktokens), target.pos)
    ref.pos <- setdiff(ref.pos, match.pos)
    ref.pos <- setdiff(ref.pos, bound.pos)
    
    t1 <- table(blocktokens[target.pos])
    blockfreqlist1 <- as.vector(t1); names(blockfreqlist1) <- names(t1)
    globfreqlist1 <- addfreqlists(globfreqlist1, blockfreqlist1)
    
    t2 <- table(blocktokens[ref.pos])
    blockfreqlist2 <- as.vector(t2); names(blockfreqlist2) <- names(t2)
    globfreqlist2 <- addfreqlists(globfreqlist2, blockfreqlist2)

    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  retval$targetfreqs <- globfreqlist1
  retval$reffreqs <- globfreqlist2
  return(retval)
}


raw.corpus.surface.cooccur.node.old <- function(
                                       fnames, 
                                       freqlist,
                                       L=3, 
                                       R=3,
                                       node.regexp,
                                       boundary.marker="<boundary />",
                                       fileEncoding="", 
                                       drop.tokens=NULL) {
  # ---
  itemlen <- length(freqlist)
  itempos <- 1:itemlen; names(itempos) <- names(freqlist)
  # ---
  item.freqs <- rep(0, length(freqlist))
  names(item.freqs) <- names(freqlist)
  # ---
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     tokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       tokens <- tokens[- grep(drop.tokens, tokens, perl=TRUE)]
     }
     node.pos <- rep(FALSE, length(tokens))
     node.pos[grep(node.regexp, tokens, perl=TRUE)] <- TRUE
     cat("."); utils::flush.console()
     # ---
     for (j in 1:length(tokens)) { # loop over collocates
       colloc <- tokens[j]
       if (! is.na(freqlist[colloc])) {
         node.found <- FALSE
         # --- look for nodes to left of collocate ---
         k <- 1
         while ((k <= R) && ((j - k) > 0)) {  # R from persp. of colloc is L from persp. of node
           node <- tokens[j - k]
           if (is.na(node)) {
             k <- R+1
           } else if (node == boundary.marker) {
             k <- R+1
           } else {
             if (node.pos[j-k]) {
               node.found <- TRUE             
             }
             k <- k+1
           }
         }  # end of while (k <= R) {  }
         # --- look for nodes to right of collocate ---
         k <- 1
         while (k <= L) {  # L from persp. of colloc is R from persp. of node
           node <- tokens[j + k]
           if (is.na(node)) {
             k <- L+1
           } else if (node == boundary.marker) {
             k <- L+1
           } else {
             if (node.pos[j+k]) {
               node.found <- TRUE
             }
             k <- k+1
           }
         }  # end of while (k <= L) {  }
         if (node.found) {
               pos <- itempos[colloc]
               item.freqs[pos] = item.freqs[pos] + 1
         }
       }  # end of: if (! is.na(freqlist[colloc]))  {  }
     }  # end of: for (j in 1:length(tokens)) {  }
     # ---
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }  # end of: if (((i %% 60) == 0) | (i == length(fnames))) {  }
  }  # end of: for (i in 1:length(fnames)) { }
  return(item.freqs)
}


surface.cooccur.stats <- function(cooccur.freq, freqlist, corpus.size,
                                  min.freq=3) {
  cooccur.freq <- cooccur.freq[cooccur.freq >= min.freq]
  d <- data.frame(fullname = names(cooccur.freq), stringsAsFactors=FALSE)
  d$colloc = gsub("^(.*) \\| .*$", 
                  "\\1", names(cooccur.freq), perl=TRUE)
  d$node <- gsub("^.* \\| (.*)$", 
                 "\\1", names(cooccur.freq), perl=TRUE)
  d$a <- cooccur.freq
  d$b <- freqlist[d$node] - d$a
  d$c <- freqlist[d$colloc] - d$a
  d$d <- corpus.size - d$a - d$b - d$c
  d$DICE <- calc.DICE(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$OR <- calc.OR(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$log.OR <- calc.log.OR(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$PMI <- calc.PMI(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$Chi2 <- calc.Chi2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()
  d$Chi2.signed <- calc.Chi2.signed(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$p.Chi2 <- calc.p.Chi2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console() # two-sided
  d$G2 <- calc.G2(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$G2.signed <- calc.G2.signed(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()
  d$p.G2 <- calc.p.G2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console() # two-sided
  d$t <- calc.t(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()      
  d$p.t <- calc.p.t(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
                                         # use of this test not well motivated!!!
                                         # one-sided 
                                         # (only detects a too high)
  d$t.as.chisq1 <- p_to_chisq1(d$p.t); cat("."); utils::flush.console()
                                                   # bring p to scale of Chi2 and G2
  d$p.fisher <- calc.p.fisher(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
                                                   # one-sided 
                                                   # (only detects a too high)
  d$fisher.as.chisq1 <- p_to_chisq1(d$p.fisher); cat("."); utils::flush.console()
                                                   # bring p to scale of Chi2/G2
  cat("\n"); utils::flush.console()
  return(d)
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
        # ----------------------------------------------------------
        # TAB is assumed to surround hit (but this is not cheched)
        # ----------------------------------------------------------
        cells <- strsplit(lines, split = "\t", fixed = TRUE)
        d <- data.frame(x = 1:length(lines)); d$x <- NULL
        d$id <- unlist(lapply(cells, "[", 1))
        d$left <- unlist(lapply(cells, "[", 2))
        d$match <- unlist(lapply(cells, "[", 3))
        d$right <- unlist(lapply(cells, "[", 4))
        d$source <- unlist(lapply(cells, "[", 5))   
    } else if (version[1] == "3.4.3") {
        # ----------------------------------------------------------
        # tab is preferred to surround hit (but may not)
        # ----------------------------------------------------------
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

read.conc.antconc.prev <- function(file, version=c("3.4.3", "3.2.4"),
  fileEncoding="") {
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
# ---
# To be tested: do I need to use readChar, as I did in
#               read.conc.antconc.old(...), or is it safe to
#               use scan(...)
# ---
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)  
  d <- NA
  if (length(lines > 0)) {
    if (version[1] == "3.2.4") {
        # ---------------------------------------------------------- 
        # TAB is assumed to surround hit (but this is not cheched) 
        # ---------------------------------------------------------- 
        cells <- strsplit(lines, split="\t", fixed=TRUE)
        d <- data.frame(x=1:length(lines)); d$x <- NULL
        d$Id <- unlist(lapply(cells, "[", 1))
        d$Left <- unlist(lapply(cells, "[", 2))
        d$Node <- unlist(lapply(cells, "[", 3))
        d$Right <- unlist(lapply(cells, "[", 4))
        d$Source <- unlist(lapply(cells, "[", 5))    
    } else if (version[1] == "3.4.3") {
        # ---------------------------------------------------------- 
        # tab is preferred to surround hit (but may not)
        # ---------------------------------------------------------- 
        cells <- strsplit(lines, split="\t", fixed=TRUE)
        d <- data.frame(x=1:length(lines)); d$x <- NULL
        d$Id <- unlist(lapply(cells, "[", 1))
        d$Left <- unlist(lapply(cells, "[", 2))
        col3 <- unlist(lapply(cells, "[", 3))
        max.n.col3 <- max(nchar(col3))
        if (max.n.col3 == 0) {
          # there are TABS around the hits in KWIC display
          d$Node <- unlist(lapply(cells, "[", 4))
          d$Right <- unlist(lapply(cells, "[", 5))
          d$Source <- unlist(lapply(cells, "[", 6))
        } else {
          # there are no TABS around the hits in KWIC display
          # we can only guess what are the boudaries of the hit
          d$Node <- NA
          d$Right <- unlist(lapply(cells, "[", 3))
          d$Node <- gsub("^(\\s*[^ ]*).*$", "\\1", d$Right, perl=TRUE)
          d$Right <- substr(d$Right, nchar(d$Node)+1, nchar(d$Right))
          d$Source <- unlist(lapply(cells, "[", 4))        
        }
    }
  }
  return(d)
}


read.conc.antconc.old <- function(file, window.size=50, fileEncoding="") {
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
# - Also, the window.size argument must specify the CORRECT window size that
#   was used in AntConc when running the query.
# - Also, you must make sure to specify the correct file encoding.
# ---
  fileSize <- file.info(file)$size
  connection <- file(file, "rb")
  chars <- iconv(readChar(connection, fileSize, useBytes=TRUE),
                 from=fileEncoding)
  close(connection)
  chars <- gsub("\r", " ", chars)
  lines <- strsplit(chars, "\n")[[1]]
  Id <- gsub("^([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  lines <- substr(lines, nchar(Id)+2, nchar(lines))
  #KWIC <- gsub("^[^\t]*\t(.*)\t[^\t]*$", "\\1", lines, perl=TRUE)
  Source <- gsub("^.*\t([^\t]*)$", "\\1", lines, perl=TRUE)
  lines <- substr(lines, 1, nchar(lines)-nchar(Source)-1)
  Left <- substr(lines, 1, window.size)
  lines <- substr(lines, window.size+1, nchar(lines))
  Node <- rep("", length(lines))
  hasHitInTabs <- grep("^\t([^\t]*)\t.*$", lines, perl=TRUE)
  hitInTabs <- gsub("^\t([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  Node[hasHitInTabs] <- hitInTabs[hasHitInTabs]
  hasHitNoTabs <- grep("^([\\w]*).*$", lines, perl=TRUE)
  hitNoTabs <- gsub("^([\\w]*).*$", "\\1", lines, perl=TRUE)
  hasHitNoTabs <- setdiff(hasHitNoTabs, hasHitInTabs)
  Node[hasHitNoTabs] <- hitNoTabs[hasHitNoTabs]
  NodeSize <- nchar(Node)+1
  NodeSize[hasHitInTabs] <- NodeSize[hasHitInTabs] + 2 
  Right <- substr(lines, NodeSize, nchar(lines))
  Left <- gsub("\t", " ", Left, perl=TRUE)
  Node <- gsub("\t", " ", Node, perl=TRUE)
  Right <- gsub("\t", " ", Right, perl=TRUE)
  return(data.frame(Id=Id, Source=Source, Left=Left, Node=Node, Right=Right))
}

read.conc.antconc.veryold <- function(file, window.size=50, fileEncoding="") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: NO
# - Also, the window.size argument must specify the CORRECT window size that
#   was used in AntConc when running the query.
# - Also, you must make sure to specify the correct file encoding.
# ---
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)
  Id <- gsub("^([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  KWIC <- gsub("^[^\t]*\t(.*)\t[^\t]*$", "\\1", lines, perl=TRUE)
  Source <- gsub("^.*\t([^\t]*)$", "\\1", lines, perl=TRUE)
  possible.antconc.errors <- (nchar(KWIC) < window.size*2)
  KWIC[possible.antconc.errors] <- format(KWIC[possible.antconc.errors], 
    width=(window.size*2 + 20)) # mitigate occasional AntConc error
  Left <- substr(KWIC, 1, window.size)
  Node <- substr(KWIC, window.size+1, nchar(KWIC)-window.size)
  Right <- substr(KWIC, nchar(KWIC)-window.size+1,nchar(KWIC))
  Left <- gsub("\t", " ", Left, perl=TRUE)
  Node <- gsub("\t", " ", Node, perl=TRUE)
  Right <- gsub("\t", " ", Right, perl=TRUE)
  return(data.frame(Id=Id, Source=Source, Left=Left, Node=Node, Right=Right))
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

conc.to.dataset.corpuseye.prev <- function(infiles, outfile) {
# ---
# - infiles are one or several files to which the concordance search 
#   results from the corpuseye website (http://corp.hum.sdu.dk/)
#   were exported (by clicking on export all and then saving as *.txt)
# - such files always have encoding UTF-8
# - we also save the results with encoding UTF-8
# ---
  corpdata <- vector()
  for (infile in infiles) {
    lines <- scan(infile, what="char", sep="\n", 
                  fileEncoding="UTF-8", blank.lines.skip = FALSE,
                  quiet=TRUE)
    corpdata <- append(corpdata, lines)
  }
  corp <- paste(corpdata, collapse=" \n")
  corp <- gsub("([^\n]) \n([^\n])", "\\1 \\2", corp, perl=TRUE)
  corp <- gsub("\n \n", "\n", corp, perl=TRUE)
  corp <- gsub("\\*", "\t", corp, perl=TRUE)
  corp <- gsub("\\|", "", corp, perl=TRUE)
  corp <- unlist(strsplit(corp, "\n"))
  corp <- corp[grep("[^\\s]", corp, perl=TRUE)]
  corp <- paste("corpuseye", corp, sep="\t")
  corp <- paste(1:length(corp),"\t", corp, sep="")
  corp <- paste(1:length(corp),"\t", corp, sep="")
  # some more appending needs to be done here
  writeLines(iconv(append("GlobId\tId\tSource\tLeft\tNode\tRight",corp),
                   to="UTF-8", mark=TRUE), 
             outfile, useBytes=TRUE)
}

conc_to_dataset_bibleworks <- function(x, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(x)) {
    data_lines <- read_txt(x[i])
    data_lines <- data_lines[nchar(data_lines) > 0]
    id_lines <- 1:length(data_lines)
    metadata_lines <- read_txt(metafiles[i])
    metadata_lines <- metadata_lines[nchar(metadata_lines) > 0]
    source_lines <- rep(x[i], length(data_lines))
    id <- append(id, id_lines)
    data <- append(data, data_lines)
    metadata <- append(metadata, metadata_lines)
    source <- append(source, source_lines)
  }
  d <- data.frame(glob_id = 1:length(data),
                  id = id,
                  source = source,
                  data = data,
                  metadata = metadata)
  d <- d[-nrow(d), ] # in a future update, this should be done more elegantly
  write_dataset(d, outfile)
  invisible(x)
}


conc.to.dataset.bibleworks.prev <- function(datafiles, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(datafiles)) {
    data.lines <- scan(datafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = TRUE,
                       quiet=TRUE)
    id.lines <- 1:length(data.lines)
    metadata.lines <- scan(metafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = TRUE,
                       quiet=TRUE)
    source.lines <- rep(datafiles[i], length(data.lines))
    id <- append(id, id.lines)
    data <- append(data, data.lines)
    metadata <- append(metadata, metadata.lines)
    source <- append(source, source.lines)
  }
  d <- data.frame(GlobId=1:length(data),
                  Id=id,
                  Source=source,
                  Data=data,
                  Metadata=metadata)
  d <- d[-nrow(d),] # in a future update, this should be done more elegantly
  write_dataset(d, outfile)
}

conc.to.dataset.bibleworks2 <- function(datafiles, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(datafiles)) {
    data.lines <- scan(datafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = FALSE,
                       quiet=TRUE)
    data.lines <- paste(data.lines, collapse=" \n")
    data.lines <- gsub("([^\n]) \n([^\n])", "\\1 \\2", data.lines, perl=TRUE)
    data.lines <- gsub("^ \n ", "", data.lines, perl=TRUE)
    data.lines <- gsub("\n \n", "\n", data.lines, perl=TRUE)
    data.lines <- unlist(strsplit(data.lines, " \n "))
    id.lines <- 1:length(data.lines)
    metadata.lines <- scan(metafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = FALSE,
                       quiet=TRUE)
    metadata.lines <- paste(metadata.lines, collapse=" \n")
    metadata.lines <- gsub("([^\n]) \n([^\n])", "\\1 \\2", metadata.lines, perl=TRUE)
    metadata.lines <- gsub("^ \n ", "", metadata.lines, perl=TRUE)
    metadata.lines <- gsub("\n \n", "\n", metadata.lines, perl=TRUE)
    metadata.lines <- unlist(strsplit(metadata.lines, " \n "))
    source.lines <- rep(datafiles[i], length(data.lines))
    id <- append(id, id.lines)
    data <- append(data, data.lines)
    metadata <- append(metadata, metadata.lines)
    source <- append(source, source.lines)
  }
  d <- data.frame(GlobId=1:length(data),
                  Id=id,
                  Source=source,
                  Data=data,
                  Metadata=metadata)
  write_dataset(d, outfile)
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



read.dataset.prev <- function(file, header=TRUE, sep="\t", 
                              quote="", comment.char="", fileEncoding="UTF-8", ...) {
# --
# reads a data frame from the file format that is most often used in mcl
#   #   (header, quote and comment.char have not been implemented yet)
# --
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(x=1:length(lines)); d$x <- NULL
  for (i in 1:length(cols)) {
      d[[cols[i]]] <- unlist(lapply(cells, "[", i))
      d[[cols[i]]] <- type.convert(d[[cols[i]]])
  }
  return(d)
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


write.dataset.prev <- function(x, file="", sep="\t", fileEncoding="UTF-8") {
# --
# writes a data frame to the file format that is most often used in mcl
# --
  if (nrow(x) > 0) {
    lines <- as.character(x[,1])
    for (i in 2:ncol(x)) {
      lines <- paste(lines, as.character(x[,i]), sep=sep)
    }
    names <- paste(names(x), collapse=sep)
    writeLines(iconv(append(names, lines), to=fileEncoding, mark=TRUE), 
               file, useBytes=TRUE)  
  }
}

# ---
#  FREQUENCY LISTS (represented as named numeric vectors)
#


freqlist_from_text <- function(x, all_freq_one = FALSE) { 
  t <- table(x)
  if (all_freq_one) {
    retval <- rep(1, length(t))
  } else {
    retval <- as.vector(t)
  }
  names(retval) <- names(t)
  class(retval) <- "freqlist"
  return(retval)
}

addfreqlists <- function(x, y) {
  sum.names <- union(names(x), names(y))
  sum.x <- x[sum.names]; sum.x[is.na(sum.x)] <- 0
  sum.y <- y[sum.names]; sum.y[is.na(sum.y)] <- 0
  sum.freq <- sum.x + sum.y
  names(sum.freq) <- sum.names
  class(sum.freq) <- "freqlist"
  return(sum.freq)
}

# the following function may eventually be turned into
# sort.freqlist(), i.e. a function that can be called
# with sort(x, ...), with x a freqlist
sortfreqlist <- function(x, criterion=c("word", "freq"), decreasing=FALSE) {
  if (criterion[1] == "freq") {
    return(sort(x, decreasing=decreasing))
  } else {
    return(x[sort(names(x), decreasing=decreasing)])
  }
}

writefreqlist <- function(x, file="", sep="\t", fileEncoding="UTF-8") {
# --
# writes a frequency list to file 
# --
  lines <- c("word\tfreq", paste(names(x), x, sep="\t"))
  writeLines(iconv(lines, to=fileEncoding, mark=TRUE), file, useBytes=TRUE)
}

readfreqlist <- function(file, header=TRUE, sep="\t", 
                         quote="", comment.char="", 
                         fileEncoding="UTF-8", ...) {
# --
# reads a frequency list from file 
#   (quote and comment.char have not been implemented yet)
# --
  # --
  con <- file(file, encoding = fileEncoding)
  lines <- readLines(con, warn=FALSE)
  close(con)    
  # --
  #lines <- scan(file, what="char", sep="\n", 
  #              fileEncoding=fileEncoding,
  #              blank.lines.skip = FALSE,
  #              quiet=TRUE, ...)
  if (header) { lines <- lines[2:length(lines)] }
  cells <- strsplit(lines, sep)
  x <- as.numeric(unlist(lapply(cells, "[", 2)))
  names(x) <- unlist(lapply(cells, "[", 1))
  return(x)
}

# the dependency on raw.getfreqlist in this function
# needs to be eliminated

read.wordlist <- function(file, header=TRUE, sep="\t", 
                          quote="", comment.char="", 
                          fileEncoding="UTF-8", ...) {
# --
# reads a word list from file 
#   (quote and comment.char have not been implemented yet)
  return(names(raw.getfreqlist(file, fileEncoding=fileEncoding)))
}



raw.getfreqlist <- function(files,
                            re.split="[.'!?;;,\\s]+",
                            re.ok.line=".*",
                            re.drop.token=NA,
                            to.lower=TRUE,
                            perl=TRUE,
                            blocksize=300,
                            verbose=FALSE,
                            dot.blocksize=10,
                            fileEncoding="latin1") {
  first.pt <- proc.time(); new.pt <- first.pt
  globfreqlist <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = fileEncoding)
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding=fileEncoding, 
      #                 blank.lines.skip=FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- unlist(strsplit(newlines, re.split, perl=perl))
      if (to.lower) { newtokens <- tolower(newtokens) }
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    t <- table(blocktokens)
    blockfreqlist <- as.vector(t); names(blockfreqlist) <- names(t)
    globfreqlist <- addfreqlists(globfreqlist, blockfreqlist)
    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  cat("\n")
  return(globfreqlist)
}

wpl.getfreqlist <- function(files, 
                            re.ok.line=".*",
                             re.token.match=".*",
                             re.token.replace="\\1",
                             re.drop.token=NA,
                             perl=TRUE,
                             blocksize=300,
                             verbose=FALSE,
                             dot.blocksize=10) {
  first.pt <- proc.time(); new.pt <- first.pt
  globfreqlist <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = "latin1")
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding="latin1", 
      #                 blank.lines.skip = FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- gsub(re.token.match, re.token.replace, 
                          newsellines, perl=TRUE)
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    t <- table(blocktokens)
    blockfreqlist <- as.vector(t); names(blockfreqlist) <- names(t)
    globfreqlist <- addfreqlists(globfreqlist, blockfreqlist)
    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  cat("\n")
  return(globfreqlist)
}


show_dot <- function(show_dots = FALSE) {
  if (show_dots) {
    cat(".")
    utils::flush.console()
  }
  invisible(show_dots)
}



# -- All calc.X() functions below in principle have become
# -- redundant, because their functionality has become subsumed by
# -- assoc_scores_abcd();
# -- they are kept nonetheless, because:
#      (i)  I'm still keeping open the option to (redundantly) also
#           export these functions (next to assoc_scores_abcd(),
#           simply because their source code is easier to grasp
#           for readers of MCLM
#      (ii) they can still be useful for debugging assoc_scores_abcd()
# -- However, for the moment, these functions are not exported.

calc.exp.a <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn
  return(ea)
}

calc.min.exp <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(apply(cbind(ea,eb,ec,ed), 1, min))
}

calc.G2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
}

calc.G2.is.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return(a/m > c/n)
}

calc.p.G2 <- function(a,b,c,d) {
  g2 <- calc.G2(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.pos <- function(a,b,c,d) {
  g2 <- calc.G2.pos(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.neg <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.neg <- function(a,b,c,d) {
  g2 <- calc.G2.neg(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)
}

calc.Chi2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
}

calc.Chi2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- ((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)  
}
calc.p.Chi2 <- function(a,b,c,d) {
  chi2 <- calc.Chi2(a,b,c,d)
  return(1 - pchisq(chi2, 1))
}

calc.PMI <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(log2( (a/mn) / ( (k/mn) * (m/mn) ) ))
}

calc.MS <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(min(a/m, a/k))
}

calc.t <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(t)
}

calc.p.t <- function(a,b,c,d) { # 1-sided
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(1 - pt(t, mn-1))
}

calc.p.t.two.sided <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  p <- 1 - pt(t, mn-1) # first assumption: positive deviation
  if (t < 0) {
    p <- pt(t, mn-1)   # correct previous assumption if needed
  }
  p <- 2*p             # two.sided
  return(p)
}


calc.DP <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) - (c/n))
}

calc.RR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) / (c/n))
}

calc.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return((a/b) / (c/d))
}

calc.log.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return(log((a/b) / (c/d)))
}

calc.DICE <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; k <- a + c
  return( (2*a)/(m+k) )
}

calc.p.fisher <- function(a,b,c,d) {
  # the use of zero_plus() doesn't seem to hinder, so we keep it
  # for the sake of conceptual consistency across the
  # calc.X() functions
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c
  return (1 - phyper(a-1,m,n,k))
}

calc.p.fisher.two.sided <- function(a,b,c,d) {
  # no zero_plus() here, to avoid warning about non-integer values
  result <- vector()
  for (i in 1:length(a)) {
    m <- matrix(c(a[i],b[i],c[i],d[i]), nrow=2, byrow=T)
    result[i] <- fisher.test(m)$p
  }
  return(result)
}


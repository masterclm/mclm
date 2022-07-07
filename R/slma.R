# =============================================================================
# the class "slma" and related classes
# =============================================================================



# ============================================================================
# slma()
# ============================================================================


as.data.frame.slma <- function(x, ...) {
  scr <- cbind(type = rownames(x$scores), x$scores)
  #scr[scr$S_abs > 0, ]
  scr <- scr[order(scr$S_abs, decreasing = TRUE), ]
  rownames(scr) <- NULL
  scr
}

strsplit_space_tokenizer <- function(x) {
    unlist(strsplit(as.character(x), "\\s+", perl = TRUE))
}    

slma_old <- function(corp_a,
                 corp_b,
                 file_encoding = "UTF-8",
                 sig_cutoff = qchisq(.95, df = 1),
                 small_pos = 0.00001,
                 keep_intermediate = TRUE) {
  if (! "Corpus" %in% class(corp_a)) {
    corp_a <- tm::VCorpus(
      tm::DirSource(corp_a,
                    encoding = file_encoding))
  }
  if (! "Corpus" %in% class(corp_b)) {
    corp_b <- tm::VCorpus(
      tm::DirSource(corp_b,
                    encoding = file_encoding))
  }  
  result <- list(intermediate = list())
  class(result) <- "slma"  
  corp_ab <- c(corp_a, corp_b)
  tdm <- tm::TermDocumentMatrix(
    corp_ab,
    control = list(tokenize = strsplit_space_tokenizer,
                   wordLengths = c(1, Inf)))
  scores <- data.frame(row.names = rownames(tdm))
  n <- length(corp_a)
  m <- length(corp_b)  
  mx_gsig <- matrix(nrow = nrow(scores), ncol = n*m)   
  mx_logor <- matrix(nrow = nrow(scores), ncol = n*m)
  mx_dir <- matrix(nrow = nrow(scores), ncol = n*m)
  mx_col_idx <- 0
  for (i in 1:n) {
    for (j in (n+1):(n+m)) {
      mx_col_idx <- mx_col_idx + 1
      a <- as.vector(tdm[, i])
      b <- sum(a) - a
      c <- as.vector(tdm[, j])
      d <- sum(c) - c
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      mx_gsig[, mx_col_idx] <- ifelse(ascores$G > sig_cutoff, 1, NA) 
      mx_logor[, mx_col_idx] <- log(ascores$OR)
      mx_dir[, mx_col_idx] <- ascores$dir
    }  
  }
  scores$S_abs <- base::rowSums((mx_dir * mx_gsig), na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(mx_dir) 
  scores$S_att <- base::rowSums((mx_dir == 1) * mx_gsig, na.rm = TRUE)
  scores$S_rep <- base::rowSums((mx_dir == -1) * mx_gsig, na.rm = TRUE)
  mx_prod <- mx_logor * mx_gsig
  scores$lor_min <- apply(mx_prod, 1, adhoc_min) 
  scores$lor_max <- apply(mx_prod, 1, adhoc_max) 
  scores$lor_sd <- apply(mx_prod, 1, sd, na.rm = TRUE) 
  scores$S_lor <- apply(mx_prod, 1, adhoc_sum) / ncol(mx_dir)
  # --
  scores <- scores[order(scores$S_lor, decreasing = TRUE), ]
  # --                               
  result$scores <- scores
  result$sig_cutoff <- sig_cutoff
  result$small_pos <- small_pos
  if (keep_intermediate) {
    result$intermediate$corp_a <- corp_a
    result$intermediate$corp_b <- corp_b
    result$intermediate$tdm <- tdm
  }  
  result
}

slma_fast <- function(x,                                        # A corpus files  
                      y,                                        # B corpus files
                      file_encoding = "UTF-8",
                      sig_cutoff = qchisq(.95, df = 1),
                      small_pos = 0.00001,
                      keep_intermediate = FALSE,
                      verbose = TRUE,
                      max_rank = 5000,
                      ngram_size = NULL,
                      ngram_sep = "_",
                      ...) {
  result <- list(scores = NULL, intermediate = NULL)
  class(result) <- c("slma", "list")  
  # --------------------------------------------------------------------------
  cat_if_verbose("building global frequency list for x\n", verbose)
  # --------------------------------------------------------------------------
  n <- length(x)
  m <- length(y)
  txt_list <- vector(mode = "list", length = n)
  flist_a <- NULL
  for (i in 1:n) {
    cat_if_verbose(".", verbose)
    txt_list[[i]] <- read_txt(x[i], file_encoding = file_encoding)
  }
  cat_if_verbose("<merging>", verbose)
  flist_a <- freqlist_char(unlist(txt_list),
                           ngram_size = ngram_size, ngram_sep = ngram_sep,
                           ...)  
  cat_if_verbose("\n", verbose)
  # --------------------------------------------------------------------------
  cat_if_verbose("building separate frequency lists for each document\n")
  # --------------------------------------------------------------------------
  sel_markers <- names(keep_bool(flist_a, ranks(flist_a) <= max_rank))
  txt_list <- NULL
  flist_a <- NULL
  freqs_list <- vector(mode = "list", length = n + m) # frequencies
  tnt_list <- vector(mode = "list", length = n + m)   # tot nr of tokens
  for (i in 1:n) {
    cat_if_verbose(".",  verbose)
    txt <- read_txt(x[i], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size, ngram_sep = ngram_sep,
                           ...)
    flist <- keep_types(flist, sel_markers)
    freqs_list[[i]] <- as.numeric(flist)
    tnt_list[[i]] <- tot_n_tokens(flist)
  }
  for (j in 1:m) {
    cat_if_verbose(".", verbose)
    txt <- read_txt(y[j], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size, ngram_sep = ngram_sep,
                           ...)
    flist <- keep_types(flist, sel_markers)
    freqs_list[[n + j]] <- as.numeric(flist)
    tnt_list[[n + j]] <- tot_n_tokens(flist)
  } 
  cat_if_verbose("\n", verbose)
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating assoc scores\n", verbose)
  # --------------------------------------------------------------------------
  scores <- vector(mode = "list")
  mx_gsig <- matrix(nrow = length(sel_markers), ncol = n*m)   
  mx_logor <- matrix(nrow = length(sel_markers), ncol = n*m)
  mx_dir <- matrix(nrow = length(sel_markers), ncol = n*m)
  mx_col_idx <- 0
  for (i in 1:n) {
    for (j in (n+1):(n+m)) {
      cat_if_verbose(".", verbose)
      mx_col_idx <- mx_col_idx + 1
      a <- freqs_list[[i]]
      b <- tnt_list[[i]] - a
      c <- freqs_list[[j]]
      d <- tnt_list[[j]] - c
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      mx_gsig[, mx_col_idx] <- ifelse(ascores$G > sig_cutoff, 1, NA) 
      mx_logor[, mx_col_idx] <- log(ascores$OR)
      mx_dir[, mx_col_idx] <- ascores$dir
    }  
  }
  cat_if_verbose("\n", verbose)  
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating stability measures\n", verbose)
  # --------------------------------------------------------------------------
  scores$S_abs <- base::rowSums((mx_dir * mx_gsig), na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(mx_dir) 
  scores$S_att <- base::rowSums((mx_dir == 1) * mx_gsig, na.rm = TRUE)
  scores$S_rep <- base::rowSums((mx_dir == -1) * mx_gsig, na.rm = TRUE)
  mx_prod <- mx_logor * mx_gsig
  scores$lor_min <- apply(mx_prod, 1, adhoc_min) 
  scores$lor_max <- apply(mx_prod, 1, adhoc_max) 
  scores$lor_sd <- apply(mx_prod, 1, sd, na.rm = TRUE) 
  scores$S_lor <- apply(mx_prod, 1, adhoc_sum) / ncol(mx_dir)
  # --------------------------------------------------------------------------
  cat_if_verbose("done\n", verbose)
  # --------------------------------------------------------------------------
  class(scores) <- "data.frame"
  rownames(scores) <- sel_markers
  # --------------------------------------------------------------------------
  result$scores <- scores
  result$sig_cutoff <- sig_cutoff
  result$small_pos <- small_pos
  result$x <- x
  result$y <- y
  if (keep_intermediate) {
    result$intermediate <- list(marker_freqs = freqs_list,
                                tot_n_tokens = tnt_list)
  }  
  result  
}

slma <- function(x,                                        # A corpus files  
                 y,                                        # B corpus files
                 file_encoding = "UTF-8",
                 sig_cutoff = qchisq(.95, df = 1),
                 small_pos = 0.00001,
                 keep_intermediate = FALSE,
                 verbose = TRUE,
                 min_rank = 1,
                 max_rank = 5000, # can be set to 0
                 keeplist = NULL,
                 stoplist = NULL,                 
                 ngram_size = NULL,
                 max_skip = 0,
                 ngram_sep = "_",
                 ngram_n_open = 0,
                 ngram_open = "[]",
                 ...) {
  result <- list(scores = NULL, intermediate = NULL)
  class(result) <- c("slma", "list")
  n <- length(x)
  m <- length(y)
  # ----------------------------------------------------------------------------
  # STEP 1: deciding upon keeplist (i.e. a list of candidate markers)
  #   - if a keeplist is given as an argument, it is accepted as is
  #        (in that case, stoplist is ignored; keeplist has precedence over
  #         stoplist)
  #   - if not, then the keeplist is derived from the arguments
  #        min_rank, max_rank, and stoplist (and obviously also from the
  #        ngram-related arguments.
  # ----------------------------------------------------------------------------
  if (is.null(keeplist)) {
    # --------------------------------------------------------------------------
    cat_if_verbose("building global frequency list for x\n", verbose)
    # --------------------------------------------------------------------------
    flist_a <- freqlist(NA) # make empty frequency list 
    for (i in 1:n) {
      cat_if_verbose(".", verbose)
      txt <- read_txt(x[i], file_encoding = file_encoding)
      flist <- freqlist_char(txt,
                             ngram_size = ngram_size,
                             max_skip = max_skip,
                             ngram_sep = ngram_sep,
                             ngram_n_open = ngram_n_open,
                             ngram_open = ngram_open,
                             ...)
      flist_a <- freqlist_merge_two(flist_a, flist)
    }
    rks <- ranks(flist_a)
    keeplist <- names(keep_bool(flist_a,
                                (rks >= min_rank) & (rks <= max_rank)))
    rm(flist_a) # flist_a no longer needed
    if (!is.null(stoplist)) {
      keeplist <- setdiff(keeplist, stoplist)
    }
    cat_if_verbose("\n", verbose)
  }
  # ----------------------------------------------------------------------------
  # STEP 2: building per-document frequency lists
  # ----------------------------------------------------------------------------
  cat_if_verbose("building separate frequency lists for each document\n")
  # --------------------------------------------------------------------------  
  freqs_list <- vector(mode = "list", length = n + m) # frequencies
  tnt_list <- vector(mode = "list", length = n + m)   # tot nr of tokens
  for (i in 1:n) {
    cat_if_verbose(".",  verbose)
    txt <- read_txt(x[i], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size,
                           max_skip = max_skip,
                           ngram_sep = ngram_sep,
                           ngram_n_open = ngram_n_open,
                           ngram_open = ngram_open,
                           ...)
    flist <- keep_types(flist, keeplist)
    freqs_list[[i]] <- as.numeric(flist)
    tnt_list[[i]] <- tot_n_tokens(flist)
  }
  cat_if_verbose("\n", verbose)
  for (j in 1:m) {
    cat_if_verbose(".", verbose)
    txt <- read_txt(y[j], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size,
                           max_skip = max_skip,
                           ngram_sep = ngram_sep,
                           ngram_n_open = ngram_n_open,
                           ngram_open = ngram_open,
                           ...)
    flist <- keep_types(flist, keeplist)
    freqs_list[[n + j]] <- as.numeric(flist)
    tnt_list[[n + j]] <- tot_n_tokens(flist)
  } 
  cat_if_verbose("\n", verbose)
  # --------------------------------------------------------------------------
  # STEP 3: calculating association scores
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating assoc scores\n", verbose)
  # --------------------------------------------------------------------------
  scores <- vector(mode = "list")
  mx_gsig <- matrix(nrow = length(keeplist), ncol = n * m)   
  mx_logor <- matrix(nrow = length(keeplist), ncol = n * m)
  mx_dir <- matrix(nrow = length(keeplist), ncol = n * m)
  mx_col_idx <- 0
  for (i in 1:n) {
    for (j in (n + 1):(n + m)) {
      cat_if_verbose(".", verbose)
      mx_col_idx <- mx_col_idx + 1
      a <- freqs_list[[i]]
      b <- tnt_list[[i]] - a
      c <- freqs_list[[j]]
      d <- tnt_list[[j]] - c
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      mx_gsig[, mx_col_idx] <- ifelse(abs(ascores[["G_signed"]]) > sig_cutoff,
                                      1, NA) 
      mx_logor[, mx_col_idx] <- log(ascores[["OR"]])
      mx_dir[, mx_col_idx] <- ascores[["dir"]]
    }  
  }
  cat_if_verbose("\n", verbose)  
  # --------------------------------------------------------------------------
  # STEP 4: calculating stability measures
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating stability measures\n", verbose)
  # --------------------------------------------------------------------------
  scores$S_abs <- base::rowSums((mx_dir * mx_gsig), na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(mx_dir) 
  scores$S_att <- base::rowSums((mx_dir == 1) * mx_gsig, na.rm = TRUE)
  scores$S_rep <- base::rowSums((mx_dir == -1) * mx_gsig, na.rm = TRUE)
  mx_prod <- mx_logor * mx_gsig
  scores$lor_min <- apply(mx_prod, 1, adhoc_min) 
  scores$lor_max <- apply(mx_prod, 1, adhoc_max) 
  scores$lor_sd <- apply(mx_prod, 1, sd, na.rm = TRUE) 
  scores$S_lor <- apply(mx_prod, 1, adhoc_sum) / ncol(mx_dir)
  # --------------------------------------------------------------------------
  cat_if_verbose("done\n", verbose)
  # --------------------------------------------------------------------------
  class(scores) <- "data.frame"
  rownames(scores) <- keeplist
  # --------------------------------------------------------------------------
  # STEP 5: building and returning results object
  # --------------------------------------------------------------------------
  result$scores <- scores
  result$sig_cutoff <- sig_cutoff
  result$small_pos <- small_pos
  result$x <- x
  result$y <- y
  if (keep_intermediate) {
    result$intermediate <- list(marker_freqs = freqs_list,
                                tot_n_tokens = tnt_list)
  }  
  result  
}

slma_prev <- function(x,                                   # A corpus files  
                 y,                                        # B corpus files
                 file_encoding = "UTF-8",
                 sig_cutoff = qchisq(.95, df = 1),
                 small_pos = 0.00001,
                 keep_intermediate = FALSE,
                 verbose = TRUE,
                 min_rank = 1,
                 max_rank = 5000, # can be set to 0
                 keeplist = NULL,
                 stoplist = NULL,                 
                 ngram_size = NULL,
                 max_skip = 0,
                 ngram_sep = "_",
                 ngram_n_open = 0,
                 ngram_open = "[]",
                 ...) {
  if (!is.null(keeplist) && !is.null(stoplist)) {
    stoplist <- setdiff(stoplist, keeplist) # keeplist takes precedence 
  }
  result <- list(scores = NULL, intermediate = NULL)
  class(result) <- c("slma", "list")  
  # --------------------------------------------------------------------------
  cat_if_verbose("building global frequency list for x\n", verbose)
  # --------------------------------------------------------------------------
  n <- length(x)
  m <- length(y)
  flist_a <- freqlist(NA) # make empty frequency list 
  for (i in 1:n) {
    cat_if_verbose(".", verbose)
    txt <- read_txt(x[i], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size,
                           max_skip = max_skip,
                           ngram_sep = ngram_sep,
                           ngram_n_open = ngram_n_open,
                           ngram_open = ngram_open,
                           ...)
    flist_a <- freqlist_merge_two(flist_a, flist)
  }
  cat_if_verbose("\n", verbose)
  # --------------------------------------------------------------------------
  cat_if_verbose("building separate frequency lists for each document\n")
  # --------------------------------------------------------------------------
  rks <- ranks(flist_a)
  sel_markers <- names(keep_bool(flist_a,
                                 (rks >= min_rank) & (rks <= max_rank)))
  rm(flist_a) # flist_a no longer needed
  if (!is.null(keeplist)) {
    sel_markers <- union(sel_markers, keeplist)
  }
  if (!is.null(stoplist)) {
    sel_markers <- setdiff(sel_markers, stoplist)
  }  
  freqs_list <- vector(mode = "list", length = n + m) # frequencies
  tnt_list <- vector(mode = "list", length = n + m)   # tot nr of tokens
  for (i in 1:n) {
    cat_if_verbose(".",  verbose)
    txt <- read_txt(x[i], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size,
                           max_skip = max_skip,
                           ngram_sep = ngram_sep,
                           ngram_n_open = ngram_n_open,
                           ngram_open = ngram_open,
                           ...)
    flist <- keep_types(flist, sel_markers)
    freqs_list[[i]] <- as.numeric(flist)
    tnt_list[[i]] <- tot_n_tokens(flist)
  }
  cat_if_verbose("\n", verbose)
  for (j in 1:m) {
    cat_if_verbose(".", verbose)
    txt <- read_txt(y[j], file_encoding = file_encoding)
    flist <- freqlist_char(txt,
                           ngram_size = ngram_size,
                           max_skip = max_skip,
                           ngram_sep = ngram_sep,
                           ngram_n_open = ngram_n_open,
                           ngram_open = ngram_open,
                           ...)
    flist <- keep_types(flist, sel_markers)
    freqs_list[[n + j]] <- as.numeric(flist)
    tnt_list[[n + j]] <- tot_n_tokens(flist)
  } 
  cat_if_verbose("\n", verbose)
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating assoc scores\n", verbose)
  # --------------------------------------------------------------------------
  scores <- vector(mode = "list")
  mx_gsig <- matrix(nrow = length(sel_markers), ncol = n*m)   
  mx_logor <- matrix(nrow = length(sel_markers), ncol = n*m)
  mx_dir <- matrix(nrow = length(sel_markers), ncol = n*m)
  mx_col_idx <- 0
  for (i in 1:n) {
    for (j in (n+1):(n+m)) {
      cat_if_verbose(".", verbose)
      mx_col_idx <- mx_col_idx + 1
      a <- freqs_list[[i]]
      b <- tnt_list[[i]] - a
      c <- freqs_list[[j]]
      d <- tnt_list[[j]] - c
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      mx_gsig[, mx_col_idx] <- ifelse(ascores$G > sig_cutoff, 1, NA) 
      mx_logor[, mx_col_idx] <- log(ascores$OR)
      mx_dir[, mx_col_idx] <- ascores$dir
    }  
  }
  cat_if_verbose("\n", verbose)  
  # --------------------------------------------------------------------------
  cat_if_verbose("calculating stability measures\n", verbose)
  # --------------------------------------------------------------------------
  scores$S_abs <- base::rowSums((mx_dir * mx_gsig), na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(mx_dir) 
  scores$S_att <- base::rowSums((mx_dir == 1) * mx_gsig, na.rm = TRUE)
  scores$S_rep <- base::rowSums((mx_dir == -1) * mx_gsig, na.rm = TRUE)
  mx_prod <- mx_logor * mx_gsig
  scores$lor_min <- apply(mx_prod, 1, adhoc_min) 
  scores$lor_max <- apply(mx_prod, 1, adhoc_max) 
  scores$lor_sd <- apply(mx_prod, 1, sd, na.rm = TRUE) 
  scores$S_lor <- apply(mx_prod, 1, adhoc_sum) / ncol(mx_dir)
  # --------------------------------------------------------------------------
  cat_if_verbose("done\n", verbose)
  # --------------------------------------------------------------------------
  class(scores) <- "data.frame"
  rownames(scores) <- sel_markers
  # --------------------------------------------------------------------------
  result$scores <- scores
  result$sig_cutoff <- sig_cutoff
  result$small_pos <- small_pos
  result$x <- x
  result$y <- y
  if (keep_intermediate) {
    result$intermediate <- list(marker_freqs = freqs_list,
                                tot_n_tokens = tnt_list)
  }  
  result  
}


print.slma <- function(x, n = 20, from = 1, ...) {
  scr <- as.data.frame(x)
  n <- min(n, nrow(scr))
  if (n > 0) {
    print(scr[1:n, ])
  }
}


details.slma <- function(x, y) {
  if (!"slma" %in% class(x)) {
    stop("x must be of class \"slma\"")
  }
  if (length(x$intermediate) == 0) {
    stop("x was not created with keep_intermediate = TRUE.")    
  }
  if (!"character" %in% typeof(y)) {
    stop("y must be a character vector.")
  }
  idx_y <- match(y, rownames(x$scores))
  if (is.na(idx_y[1])) {
    result <- NULL
  } else {
    result <- list(); class(result) <- "details.slma"
    result$summary <- x$scores[idx_y, , drop = FALSE]
    freqs <- unlist(lapply(x$intermediate$marker_freqs, "[", idx_y))
    corp_sizes <- unlist(x$intermediate$tot_n_tokens)
    labs <- c(x$x, x$y)
    n <- length(x$x)
    m <- length(x$y)
    idx <- 0
    res <- data.frame(comp = character(n * m),
                      a = numeric(n * m),
                      b = numeric(n * m),
                      c = numeric(n * m),
                      d = numeric(n * m),
                      G = numeric(n * m),
                      sig = numeric(n * m),
                      dir = numeric(n * m),                     
                      dir_sig = numeric(n * m),                     
                      log_OR = numeric(n * m),
                      log_OR_sig = numeric(n * m),                    
                      stringsAsFactors = FALSE)
    for (i in 1:n) {
      for (j in (n + 1):(n + m)) {
        idx <- idx + 1
        res[idx, "comp"] <- paste0(labs[i], "--", labs[j])
        res[idx, "a"]    <- freqs[i]
        res[idx, "b"] <- corp_sizes[i] - freqs[i]
        res[idx, "c"] <- freqs[j]
        res[idx, "d"] <- corp_sizes[j] - freqs[j]
        ascores <- assoc_abcd(res[idx, "a"],  res[idx, "b"],
                              res[idx, "c"],  res[idx, "d"],
                              small_pos = x$small_pos)
        G <- abs(ascores[["G_signed"]])
        res[idx, "G"] <- G
        res[idx, "sig"] <- ifelse(G, 1, 0)
        res[idx, "dir"] <- ascores$dir      
        res[idx, "dir_sig"]  <-  ifelse(G > x$sig_cutoff,
                                       ascores$dir,
                                       NA)      
        res[idx, "log_OR"]  <-  log(ascores$OR)
        res[idx, "log_OR_sig"]  <-  ifelse(G > x$sig_cutoff,
                                           log(ascores$OR),
                                           NA)
      }
    }
    row.names(res) <- res$comp
    res$comp <- NULL
    result$scores <- res
    result$item <- y
    result$sig_cutoff <- x$sig_cutoff
    result$small_pos <- x$small_pos
  }
  result    
}

print.details.slma <- function(x, ...) {
  if (!"details.slma" %in% class(x)) {
    stop("x must be of class \"details.slma\"")
  }  
  cat(paste0('SLMA details\n'))  
  cat(paste0('[item: "', x$item,'"]\n\n'))
  print(round(x$summary, 3))  
  cat(paste0('\n[cutoff for G: ', round(x$sig_cutoff, 3), ']\n'))
  cat(paste0('[small positive offset: ', x$small_pos, ']\n\n'))
  print(round(x$scores, 3))      
}


as.data.frame.details.slma <- function(x, ...) {
  if (! "details.slma" %in% class(x)) {
    stop("x must be of class \"details.slma\"")
  }    
  x$scores
}

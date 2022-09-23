# Constructor ==================================================================

#' Stable lexical marker analysis
#' 
#' This function conducts a stable lexical marker analysis.
#' 
#' A stable lexical marker analysis of the *A*-documents in `x` versus the *B*-documents
#' in `y` starts from a separate keyword analysis for all possible document couples
#' \eqn{(a,b)}, with *a* an *A*-document and *b* a *B*-document. If there are *n*
#' *A*-documents and *m* *B*-documents, then \eqn{n*m} keyword analyses are
#' conducted. The 'stability' of a linguistic item *x*, as a marker for the
#' collection of *A*-documents (when compared to the *B*-documents) corresponds
#' to the frequency and consistency with which *x* is found to be a keyword for
#' the *A*-documents across all aforementioned keyword analyses.
#' 
#' In any specific keyword analysis, *x* is considered a keyword for an *A*-document
#' if `G_signed` is positive and moreover `p_G` is less than `sig_cutoff`
#' (see [assoc_scores()] for more information on the measures). Item *x* is
#' considered a keyword for the *B*-document if `G_signed` is negative and moreover
#' `p_G` is less than `sig_cutoff`.
#'
#' @param x,y Character vector or [`fnames`] object with filenames for the two
#'   sets of documents.
#' @param file_encoding Encoding of all the files to read. 
#' @param sig_cutoff Numeric value indicating the cutoff value for 'significance
#'   in the stable lexical marker analysis. The default value is `qchist(.95, df = 1)`,
#'   which is about `r formatC(qchisq(.95, df = 1), digits = 3)`.
#' @param keep_intermediate Logical. If `TRUE`, results from intermediate
#'   calculations are kept in the output as the "intermediate" element. This is
#'   necessary if you want to inspect the object with the [details()] method.
#' @param verbose Logical. Whether progress should be printed to the console
#'   during analysis.
#' @param min_rank,max_rank Minimum and maximum frequency rank in the first
#'   corpus (`x`) of the items to take into consideration as candidate stable
#'   markers. Only tokens or token n-grams with a frequency rank greater than or
#'   equal to `min_rank` and lower than or equal to `max_rank` will be included.
#' @param keeplist List of types that must certainly be included in the list of
#'   candidate markers regardless of their frequency rank and of `stoplist`.
#' @param stoplist List of types that must not be included in the list of candidate
#'   markers, although, if a type is included in `keeplist`, its inclusion in
#'   `stoplist` is disregarded.
#' @inheritParams tokens
#' @inheritParams assoc_abcd
#' @param ... Additional arguments.
#'
#' @return An object of class `slma`, which is a named list with at least the following
#'   elements:
#'   
#'   - A `scores` dataframe with information about the stability of the chosen
#'   lexical items. (See below.)
#'   - An `intermediate` list with a register of intermediate values if
#'   `keep_intermediate` was `TRUE`.
#'   - Named items registering the values of the arguments with the same name,
#'   namely `sig_cutoff`, `small_pos`, `x`, and `y`.
#'   
#'   The `slma` object has [as_data_frame()] and [`print`][print.slma()] methods
#'   as well as an ad-hoc [details()] method. Note that the [`print`][print.slma()]
#'   method simply prints the main dataframe.
#'   
#'   ## Contents of the `scores` element
#'   
#'   The `scores` element is a dataframe of which the rows are linguistic items
#'   for which a stable lexical marker analysis was conducted and the columns are
#'   different 'stability measures' and related statistics. By default, the
#'   linguistic items are sorted by decreasing 'stability' according to the `S_lor`
#'   measure.
#'   
#'   | Column | Name | Computation | Range of values |
#'   | ------ | ---- | ----------- | --------------- |
#'   | `S_abs`| Absolute stability | `S_att` - `S_rep` | \eqn{-(n*m)} -- \eqn{(n*m)} |
#'   | `S_nrm`| Normalized stability | `S_abs` / \eqn{n*m} | -1 -- 1 |
#'   | `S_att`| Stability of attraction | Number of \eqn{(a,b)} couples in which the linguistic item is a keyword for the *A*-documents | 0 -- \eqn{n*m} |
#'   | `S_rep`| Stability of repulsion | Number of \eqn{(a,b)} couples in which the linguistic item is a keyword for the *B*-documents | 0 -- \eqn{n*m} |
#'   | `S_lor`| Log of odds ratio stability | Mean of `log_OR` across all \eqn{(a,b)} couples but setting to 0 the value when `p_G` is larger than `sig_cutoff` | |
#'   
#'   `S_lor` is then computed as a fraction with as its numerator the sum of all
#'   `log_OR` values across all \eqn{(a,b)} couples for which `p_G` is lower than
#'   `sig_cutoff` and as its denominator \eqn{n*m}.
#'   For more on `log_OR`, see the Value section on on [assoc_scores()]. The final
#'   three columns on the output are meant as a tool in support of the interpretation
#'   of the `log_OR` column. Considering all \eqn{(a,b)} couples for which
#'   `p_G` is smaller than `sig_cutoff`, `lor_min`, `lor_max` and `lor_sd`
#'   are their minimum, maximum and standard deviation for each element.
#'   
#' @export
#' @examples 
#' a_corp <- get_fnames(system.file("extdata", "cleveland", package = "mclm"))
#' b_corp <- get_fnames(system.file("extdata", "roosevelt", package = "mclm"))
#' slma_ex <- slma(a_corp, b_corp)
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
  n <- length(x)
  m <- length(y)
  # STEP 1: deciding upon keeplist (i.e. a list of candidate markers)
  #   - if a keeplist is given as an argument, it is accepted as is
  #        (in that case, stoplist is ignored; keeplist has precedence over
  #         stoplist)
  #   - if not, then the keeplist is derived from the arguments
  #        min_rank, max_rank, and stoplist (and obviously also from the
  #        ngram-related arguments.
  
  if (is.null(keeplist)) {
    
    cat_if_verbose("building global frequency list for x\n", verbose)
    
    flist_a <- freqlist(NA) # make empty frequency list 
    for (i in seq_along(x)) {
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
  
  # STEP 2: building per-document frequency lists --
  
  cat_if_verbose("building separate frequency lists for each document\n", verbose)
  
  freqs_list <- vector(mode = "list", length = n + m) # frequencies
  tnt_list <- vector(mode = "list", length = n + m)   # tot nr of tokens
  for (i in seq_along(x)) {
    show_dot(verbose)
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
  for (j in seq_along(y)) {
    show_dot(verbose)
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
  
  # STEP 3: calculating association scores --
  
  cat_if_verbose("calculating assoc scores\n", verbose)
  
  scores <- vector(mode = "list")
  mx_gsig <- matrix(nrow = length(keeplist), ncol = n * m)   
  mx_logor <- matrix(nrow = length(keeplist), ncol = n * m)
  mx_dir <- matrix(nrow = length(keeplist), ncol = n * m)
  mx_col_idx <- 0
  for (i in seq_along(x)) {
    for (j in (n + 1):(n + m)) {
      cat_if_verbose(".", verbose)
      mx_col_idx <- mx_col_idx + 1
      a <- freqs_list[[i]]
      b <- tnt_list[[i]] - a
      c <- freqs_list[[j]]
      d <- tnt_list[[j]] - c
      # TODO adapt strategies to deal with zeros as assoc_abcd does
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      mx_gsig[, mx_col_idx] <- ifelse(abs(ascores[["G_signed"]]) > sig_cutoff,
                                      1, NA) 
      mx_logor[, mx_col_idx] <- log(ascores[["OR"]])
      mx_dir[, mx_col_idx] <- ascores[["dir"]]
    }  
  }
  cat_if_verbose("\n", verbose)  
  # STEP 4: calculating stability measures --
  
  cat_if_verbose("calculating stability measures\n", verbose)
  
  scores$S_abs <- base::rowSums((mx_dir * mx_gsig), na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(mx_dir) 
  scores$S_att <- base::rowSums((mx_dir == 1) * mx_gsig, na.rm = TRUE)
  scores$S_rep <- base::rowSums((mx_dir == -1) * mx_gsig, na.rm = TRUE)
  mx_prod <- mx_logor * mx_gsig
  scores$S_lor <- apply(mx_prod, 1, adhoc_sum) / ncol(mx_dir)
  scores$lor_min <- apply(mx_prod, 1, adhoc_min) 
  scores$lor_max <- apply(mx_prod, 1, adhoc_max) 
  scores$lor_sd <- apply(mx_prod, 1, sd, na.rm = TRUE) 
  
  cat_if_verbose("done\n", verbose)
  
  class(scores) <- "data.frame"
  rownames(scores) <- keeplist
  
  # STEP 5: building and returning results object --
  
  result <- list(
    scores = scores,
    intermediate = NULL,
    sig_cutoff = sig_cutoff,
    small_pos = small_pos,
    x = x, y = y)
  
  class(result) <- c("slma", "list")
  
  if (keep_intermediate) {
    result$intermediate <- list(marker_freqs = freqs_list,
                                tot_n_tokens = tnt_list)
  }  
  result  
}
# S3 methods from mclm =========================================================
#' @rdname details
#' @exportS3Method details slma
details.slma <- function(x, y, shorten_names = TRUE, ...) {
  if (length(x$intermediate) == 0) {
    stop("x was not created with keep_intermediate = TRUE.")    
  }
  if (!"character" %in% typeof(y)) {
    stop("y must be a character vector.")
  }
  idx_y <- match(y, rownames(x$scores))
  if (is.na(idx_y[1])) {
    return(NULL)
  } 
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
      res[idx, "comp"] <- if (shorten_names) {
        paste0(short_names(labs[i]), "--", short_names(labs[j]))
      } else {
        paste0(labs[i], "--", labs[j])
      }
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
  
  result <- list(
    summary = x$scores[idx_y, , drop = FALSE],
    scores = res,
    item = y,
    sig_cutoff = x$sig_cutoff,
    small_pos = x$small_pos
  )
  class(result) <- "details.slma"
  result
}

#' @exportS3Method print details.slma
print.details.slma <- function(x, ...) {
  cat(paste0('SLMA details\n'))  
  cat(paste0('[item: "', x$item,'"]\n\n'))
  print(round(x$summary, 3))  
  cat(paste0('\n[cutoff for G: ', round(x$sig_cutoff, 3), ']\n'))
  cat(paste0('[small positive offset: ', x$small_pos, ']\n\n'))
  print(round(x$scores, 3))      
}

#' @rdname as_data_frame
#' @exportS3Method as.data.frame details.slma
as.data.frame.details.slma <- function(x, ...) {
  if (! "details.slma" %in% class(x)) {
    stop("x must be of class \"details.slma\"")
  }    
  x$scores
}

# S3 methods from other packages ===============================================

#' @rdname as_data_frame
#' @exportS3Method as.data.frame slma
as.data.frame.slma <- function(x, ...) {
  scr <- cbind(type = rownames(x$scores), x$scores)
  #scr[scr$S_abs > 0, ]
  scr <- scr[order(scr$S_abs, decreasing = TRUE), ]
  rownames(scr) <- NULL
  scr
}

#' @exportS3Method tibble::as_tibble slma
as_tibble.slma <- function(x, ...) {
  as_tibble(as.data.frame(x))
}

#' @rdname mclm_print
#' @exportS3Method print slma
print.slma <- function(x, n = 20, from = 1, ...) {
  scr <- as.data.frame(x)
  n <- min(n, nrow(scr))
  if (n > 0) {
    print(scr[1:n, ])
  }
}

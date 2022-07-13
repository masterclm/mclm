# TODO implement sort function
# TODO check as.data.frame
# TODO do a sort as the last step in assoc_scores


# All functions in this section assume the data stem from frequency tables of 
# the form:
#                           target item  other item
#         target context              a           b
#          other context              c           d 
#
# Moreover all functions accept a,b,c,d to be equal sized vectors of length
# 1 or higher. They take a[1],b[1],c[1] and d[1] to stem from frequency table 1, 
# a[2],b[2],c[2] and d[2], to stem from frequency table 2, etc.

# Create cooc_info ===============================================

# constructor for an object of the class "cooc_info"
cooc_info <- function(target_freqlist,
                      ref_freqlist) {
  retval <- list(target_freqlist = target_freqlist,
                 ref_freqlist = ref_freqlist)
  class(retval) = "cooc_info"
  retval
}

surf_cooc <- function(x, 
                      re_node,
                      w_left = 3, 
                      w_right = 3,
                      re_boundary = NULL,
                      re_drop_line = NULL,
                      line_glue = NULL, 
                      re_cut_area = NULL,
                      re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
                      re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
                      re_drop_token = NULL,
                      re_token_transf_in = NULL,
                      token_transf_out = NULL,
                      token_to_lower = TRUE,
                      perl = TRUE,
                      blocksize = 300,
                      verbose = FALSE,
                      dot_blocksize = 10,
                      file_encoding = "UTF-8") {
  first_pt <- proc.time(); new_pt <- first_pt
  retval <- list()
  globfreqlist1 <- freqlist(NA) # empty frequency list
  globfreqlist2 <- freqlist(NA) # empty frequency list
  n_texts <- length(x)
  if (length(file_encoding) < n_texts) {
    file_encoding <- rep(file_encoding, length = n_texts)
  }   
  i = 1
  while (i <= n_texts) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i + j) <= length(x))) {
      file <- x[i + j]
      # -- read file
      newlines <- read_txt(file, 
                           file_encoding = file_encoding[i + j])
      # -- drop lines if needed
      if (! is.null(re_drop_line) && ! is.na(re_drop_line[[1]])) {
        newlines <- newlines[grep(re_drop_line[[1]], newlines,
                                  perl = perl, invert = TRUE)]
      }
      # -- paste lines in long line if needed
      if (! is.null(line_glue)) {
        newlines <- paste(newlines, collapse = line_glue)
      }
      # -- drop uninterestion regions if needed
      if (! is.null(re_cut_area) && ! is.na(re_cut_area[[1]])) {
        newlines <- gsub(re_cut_area[[1]], "", newlines, perl = perl)
      }
      # -- identify tokens
      if (! is.null(re_token_splitter) && ! is.na(re_token_splitter[[1]])) {
        newtokens <- unlist(strsplit(newlines,
                                     re_token_splitter[[1]],
                                     perl = perl))
      } else {
        m <- gregexpr(re_token_extractor[[1]], newlines, perl = perl)
        newtokens <- unlist(regmatches(newlines, m))
      }
      # -- drop token if needed
      if (! is.null(re_drop_token) && ! is.na(re_drop_token[[1]])) {
        newtokens <- newtokens[grep(re_drop_token[[1]], newtokens,
                                    perl = perl, invert = TRUE)]
      }
      # -- transform tokens if needed
      if (! is.null(re_token_transf_in) && ! is.na(re_token_transf_in[[1]])) {
        newtokens <- gsub(re_token_transf_in[[1]], token_transf_out[[1]],
                          newtokens, perl = perl)
      }
      # -- tokens to lower if needed
      if (token_to_lower) {
        newtokens <- tolower(newtokens)
      }
      
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat(".")
        utils::flush.console() 
      }
      j <- j + 1
    }
    match_pos <- grep(re_node[[1]], blocktokens, perl = perl)
    bound_pos <- vector()
    if (!is.null(re_boundary) && !is.na(re_boundary[[1]])) {
      bound_pos <- grep(re_boundary[[1]], blocktokens, perl = perl)
    }
    target_pos <- vector()
    new_pos <- match_pos
    if (w_left > 0) {
      for (k in 1:w_left) {
        new_pos <- new_pos - 1
        new_pos <- setdiff(new_pos, bound_pos)
        target_pos <- union(target_pos, new_pos)
      }
    }
    new_pos <- match_pos
    if (w_right > 0) {
      for (k in 1:w_right) {
        new_pos <- new_pos + 1
        new_pos <- setdiff(new_pos, bound_pos)
        target_pos <- union(target_pos, new_pos)
      }
    }
    target_pos <- target_pos[target_pos > 0]
    target_pos <- target_pos[target_pos <= length(blocktokens)]
    target_pos <- setdiff(target_pos, match_pos)
    
    ref_pos <- setdiff(1:length(blocktokens), target_pos)
    ref_pos <- setdiff(ref_pos, match_pos)
    ref_pos <- setdiff(ref_pos, bound_pos)
    
    t1 <- table(blocktokens[target_pos])
    blockfreqlist1 <- as_freqlist(t1)
    globfreqlist1 <- freqlist_merge(globfreqlist1, blockfreqlist1)
    
    t2 <- table(blocktokens[ref_pos])
    blockfreqlist2 <- as_freqlist(t2)
    globfreqlist2 <- freqlist_merge(globfreqlist2, blockfreqlist2)
    
    prev_pt <- new_pt; new_pt <- proc.time()
    if (verbose) {
      cat((i + j) - 1,"(", new_pt[3] - first_pt[3], "|", 
          new_pt[3] - prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  cooc_info(target_freqlist = globfreqlist1,
            ref_freqlist = globfreqlist2)
}

text_cooc <- function(x, 
                      re_node,
                      re_boundary = NULL,
                      re_drop_line = NULL,
                      line_glue = NULL,
                      re_cut_area = NULL,
                      re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
                      re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
                      re_drop_token = NULL,
                      re_token_transf_in = NULL,
                      token_transf_out = NULL,
                      token_to_lower = TRUE,
                      perl = TRUE,
                      blocksize = 300,
                      verbose = FALSE,
                      dot_blocksize = 10,
                      file_encoding = "UTF-8") {
  first_pt <- proc.time(); new_pt <- first_pt
  retval <- list()
  globfreqlist1 <- vector()
  corpsize1 <- 0
  globfreqlist2 <- vector()
  corpsize2 <- 0
  n_texts <- length(x)
  if (length(file_encoding) < n_texts) {
    file_encoding <- rep(file_encoding, length = n_texts)
  }   
  i = 1
  while (i <= n_texts) {
    j = 0
    blocktokens1 <- vector()
    blocktokens2 <- vector()
    while ((j < blocksize) && ((i + j) <= length(x))) {
      file <- x[i + j]
      # -- read file
      newlines <- read_txt(file, 
                           file_encoding = file_encoding[i + j])
      # -- drop lines if needed
      if (! is.null(re_drop_line) && ! is.na(re_drop_line[[1]])) {
        newlines <- newlines[grep(re_drop_line[[1]], newlines,
                                  perl = perl, invert = TRUE)]
      }
      # -- paste lines in long line if needed
      if (! is.null(line_glue)) {
        newlines <- paste(newlines, collapse = line_glue)
      }
      # -- drop uninterestion regions if needed
      if (! is.null(re_cut_area) && ! is.na(re_cut_area[[1]])) {
        newlines <- gsub(re_cut_area[[1]], "", newlines, perl = perl)
      }
      # -- identify tokens
      if (! is.null(re_token_splitter) && ! is.na(re_token_splitter[[1]])) {
        newtokens <- unlist(strsplit(newlines,
                                     re_token_splitter[[1]],
                                     perl = perl))
      } else {
        m <- gregexpr(re_token_extractor[[1]], newlines, perl = perl)
        newtokens <- unlist(regmatches(newlines, m))
      }
      # -- drop token if needed
      if (! is.null(re_drop_token) && ! is.na(re_drop_token[[1]])) {
        newtokens <- newtokens[grep(re_drop_token[[1]], newtokens,
                                    perl = perl, invert = TRUE)]
      }
      # -- transform tokens if needed
      if (! is.null(re_token_transf_in) && ! is.na(re_token_transf_in[[1]])) {
        newtokens <- gsub(re_token_transf_in[[1]],
                          token_transf_out,
                          newtokens)
      }
      # -- tokens to lower if needed
      if (token_to_lower) {
        newtokens <- tolower(newtokens)
      }
      
      if (! is.null(re_boundary) && ! is.na(re_boundary[[1]])) {
        boundaries <- grep(re_boundary[[1]], newtokens, perl = perl)
      } else {
        boundaries <- numeric(0)
      }
      if (length(boundaries) > 0) {
        v <- newtokens[-boundaries]
        f <- rep(1:(length(boundaries) + 1),
                 c(boundaries, length(newtokens) + 1) -
                   c(1, boundaries + 1))
        txts <- split(v, f)
      } else {
        txts <- list('1' = newtokens)
      }
      for (item in txts) {
        hits <- grep(re_node, item, perl = perl)
        if (length(hits) > 0) {
          blocktokens1 <- c(blocktokens1, names(freqlist(item[-hits])))
          corpsize1 <-  corpsize1 + 1
        } else {
          blocktokens2 <- c(blocktokens2, names(freqlist(item)))
          corpsize2 <-  corpsize2 + 1
        }
      }
      
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat(".")
        utils::flush.console() 
      }
      j <- j + 1
    }
    # --
    t1 <- table(blocktokens1)
    blockfreqlist1 <- as.vector(t1)
    names(blockfreqlist1) <- names(t1)
    globfreqlist1 <- addfreqlists(globfreqlist1, blockfreqlist1)
    
    t2 <- table(blocktokens2)
    blockfreqlist2 <- as.vector(t2)
    names(blockfreqlist2) <- names(t2)
    globfreqlist2 <- addfreqlists(globfreqlist2, blockfreqlist2)
    
    # --
    prev_pt <- new_pt; new_pt <- proc.time()
    if (verbose) {
      cat((i + j) - 1,"(", new_pt[3] - first_pt[3], "|", 
          new_pt[3] - prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  tot_n_tokens(globfreqlist1) <- corpsize1
  tot_n_tokens(globfreqlist2) <- corpsize2
  cooc_info(target_freqlist = globfreqlist1,
            ref_freqlist = globfreqlist2)
}

# Create assoc_scores ============================================

# function that returns association scores on the basis of
# a target frequency list and a reference frequency list;
# it internally calls assoc_scores_abcd()
# is x is of class cooc_info, then y is ignored
# otherwise x and y are assumed to be target.freqlist and
# ref.freqlist respectively.
assoc_scores <- function(x, 
                         y = NULL, 
                         min_freq = 3,
                         measures = NULL,
                         with_variants = FALSE,
                         show_dots = FALSE,
                         p_fisher_2 = FALSE,
                         haldane = TRUE, # Haldane-Anscombe correction
                         small_pos = 0.00001) {
  # -- min_freq is minimum frequency in target_freqlist
  #    for inclusion in the output
  
  # TODO: sort items as last step:
  #    that will be a crucial difference
  #    between assoc_scores() and assoc_abcd(); the former sorts,
  #    the latter respects the order of the items in the input.
  
  if (!"cooc_info" %in% class(x)) {
    x <- cooc_info(target_freqlist = x, ref_freqlist = y)
  }
  if (min_freq == 0) {
    union_names <- union(names(x$target_freqlist),
                         names(x$ref_freqlist))
    x$target_freqlist <- keep_types(x$target_freqlist, union_names)
  } else {
    x$target_freqlist <- keep_bool(x$target_freqlist,
                                   x$target_freqlist >= min_freq)
  }
  x$ref_freqlist <- keep_types(x$ref_freqlist, names(x$target_freqlist))
  a <- as.numeric(x$target_freqlist)
  c <- as.numeric(x$ref_freqlist)
  assoc_abcd(a = a,
             b = tot_n_tokens(x$target_freqlist) - a,
             c = c,
             d = tot_n_tokens(x$ref_freqlist) - c,
             types = names(x$target_freqlist),
             measures = measures,
             with_variants = with_variants,
             show_dots = show_dots,
             p_fisher_2 = p_fisher_2,
             haldane = haldane,
             small_pos = small_pos)
}

# function that returns association scores on the basis of
# the frequencies a, b, c and d
assoc_abcd <- function(a, b, c, d,
                       types = NULL,
                       measures = NULL,
                       with_variants = FALSE,
                       show_dots = FALSE,
                       p_fisher_2 = FALSE,
                       haldane = TRUE,
                       small_pos = 0.00001) {
  if (is.null(types) && length(a) > 0) {
    types <- as.character(seq_along(a))
    types <- paste0("t", stringi::stri_pad_left(types,
                                                width = max(nchar(types)),
                                                pad = "0"))
  }
  if (is.null(measures)) {
    measures <- c("exp_a", "DP_rows", "RR_rows",
                  "OR", "MS", "PMI", "Dice",
                  "G_signed", "chi2_signed", "t", "fisher")
  }
  if (haldane) {
    a[a < 0] <- 0
    b[b < 0] <- 0
    c[c < 0] <- 0
    d[d < 0] <- 0
    has_zero <- (a == 0) | (b == 0) | (c == 0) | (d == 0)
    a[has_zero] <- a[has_zero] + 0.5
    b[has_zero] <- b[has_zero] + 0.5
    c[has_zero] <- c[has_zero] + 0.5
    d[has_zero] <- d[has_zero] + 0.5
  } else {
    a <- zero_plus(a, small_pos = small_pos)
    b <- zero_plus(b, small_pos = small_pos)
    c <- zero_plus(c, small_pos = small_pos)
    d <- zero_plus(d, small_pos = small_pos)
  }
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; N <- m + n
  ea <- (m * k)/N; eb <- (m * l)/N 
  ec <- (n * k)/N; ed <- (n * l)/N
  
  
  retval <- list(a = a, b = b, c = c, d = d) # removes zeroes from
  # output also
  retval$dir <- ifelse(a/m < c/n, -1, 1)     # direction
  
  compute_measure <- function(measure_names, expr, just_assign = TRUE, triple = FALSE) {
    if (triple) { # for significance statistics with signed version and p-value
      measure_names <- c(measure_names, paste0(measure_names, "_signed"))
      just_assign <- TRUE
    }
    
    if (!any(is.element(c(measure_names, "ALL"), measures))) { # measure is not called
      return()
    }
    
    if (!just_assign) { # just run code in expr
      expr
      show_dot(show_dots)
      return()
    }
    
    main_name <- measure_names[[1]]
    retval[[main_name]] <<- expr
    
    if (triple) {
      retval[[paste0(main_name, "_signed")]] <<- retval[[main_name]] * retval$dir
      
      if (with_variants) {
        retval[[paste0("p_", main_name)]] <<- 1 - pchisq(retval[[main_name]], 1)
      } else {
        if (!any(is.element(c(main_name, "ALL"), measures))) {
          retval[[main_name]] <<- NULL
        }
      }
    }
    show_dot(show_dots)
  }
  # Expected values ==
  # -- exp_a --
  compute_measure(c("exp_a", "expected"), ea)
  # -- exp_b --
  compute_measure(c("exp_b", "expected"), eb)
  # -- exp_c --
  compute_measure(c("exp_c", "expected"), ec)
  # -- exp_d --
  compute_measure(c("exp_d", "expected"), ed)
  
  # Measures ==
  # -- DP_rows, difference of proportions (aka delta p) --
  compute_measure(c("DP_rows", "DP"), (a / m) - (c / n))
  # -- DP_cols, difference of proportions (aka delta p) --
  compute_measure(c("DP_cols", "DP"), (a / k) - (b / l))
  # -- perc_DIFF_rows, %DIFF --
  compute_measure(
    c("perc_DIFF_rows", "perc_DIFF"),
    100 * (a / m - c / n) / (c / n) 
    )
  # -- perc_DIFF_cols, %DIFF --
  compute_measure(
    c("perc_DIFF_cols", "perc_DIFF"),
    100 * (a / k - b / l) / (b / l) 
  )
  # -- DC_rows, difference coefficient --
  compute_measure(
    c("DC_rows", "DC"),
    (a/m - c/n) / (a / m + c / n)
  )
  # -- DC_cols, difference coefficient --
  compute_measure(
    c("DC_cols", "DC"),
    (a/k - b/l) / (a / k + b / l)
  )
  
  # -- RR_rows, relative risk --
  compute_measure(c("RR_rows", "RR"), (a/m) / (c/n))
  # -- RR_cols, relative risk --
  compute_measure(c("RR_cols", "RR"), (a/k) / (b/l))
  # -- LR_rows, Hardie's Log Ratio (rows) --
  compute_measure(c("LR_rows", "LR"), log2((a / m) / (c / n)))
  # -- LR_cols, Hardie's Log Ratio (cols) --
  compute_measure(c("LR_cols", "LR"), log2((a / k) / (b / l)))
  # -- OR, odds ratio --
  compute_measure("OR", (a / b) / (c / d)) # also equals (a / c) / (b / d)
  # -- log_OR --
  compute_measure("log_OR", log((a / b) / (c / d)))
  
  # -- MS, minimum sensitivity --
  compute_measure("MS", pmin(a / m, a / k))
  # -- Jaccard --
  compute_measure("Jaccard", a/(m + k -a))
  # -- Dice --
  compute_measure("Dice", (2 * a) / (m + k))
  # -- logDice --
  compute_measure("logDice", 14 + log2((2 * a)/(m + k)))
  # -- phi (Cramer's V) --
  compute_measure("phi", (a * d - b * c) / sqrt((a + b) * (c + d) * (a + c) * (b + d)))
  # -- Q (Yule's Q) --
  compute_measure("Q", (a * d - b * c) / (a * d + b * c))
  # -- mu --
  compute_measure("mu", a / ea)
  # -- PMI --
  compute_measure("PMI", log2((a / N) / ((k / N) * (m / N)))) # is log van mu
  # -- pos.PMI --
  compute_measure(
    "pos_PMI",
    {
      retval$pos_PMI <- log2((a / N) / ((k / N) * (m / N)))
      retval$pos_PMI[retval$pos_PMI < 0] <- 0 # remove negs  
    },
    just_assign = FALSE)
  # -- PMI2 --
  compute_measure("PMI2", log2(((a^2) / N) / ((k / N) * (m / N))) )
  # -- PMI3 --
  compute_measure("PMI3", log2(((a^3) / N) / ((k / N) * (m / N))) )
  
  # -- chi2 (4-term) --
  compute_measure("chi2",
                  expr = (a - ea)^2 / ea + (b - eb)^2 / eb +
        (c - ec)^2 / ec + (d - ed)^2 / ed,
        triple = TRUE
        )
  # -- chi2 (4-term) with Yates correction --
  compute_measure("chi2_Y",
                  expr = (abs(a - ea) - .5)^2 / ea + (abs(b - eb) - .5)^2 / eb +
                    (abs(c - ec) - .5)^2 / ec + (abs(d - ed) - .5)^2 / ed,
                  triple = TRUE
                  )
  # -- chi2 (2-term) --
  compute_measure("chi2_2T",
                  expr = (a - ea)^2 / ea + (c - ec)^2 / ec,
                  triple = TRUE
                  )
  # -- chi2 (2-term) with Yates correction --
  compute_measure("chi2_2T_Y",
                  expr = (abs(a - ea) - .5)^2 / ea + (abs(c - ec) - .5)^2 / ec,
                  triple = TRUE
                  )
  # -- G (4-term) --
  # NOTE I call this G, but often in linguistics the name G2 is used
  compute_measure("G",
                  expr = 2 * (a * log(a / ea) + b * log(b / eb) +
                                c * log(c / ec) + d * log(d / ed)),
                  triple = TRUE
                  )
  # -- G (2-term) --
  compute_measure("G_2T",
                  expr = 2 * (a * log(a / ea) + c * log(c / ec)),
                  triple = TRUE
                  )
  # -- t --
  compute_measure("t", just_assign = FALSE, expr = {
    retval$t <- ((a / N - k / N * m / N) /
                   sqrt(((a / N) * (1 - a / N)) / N))
    if (with_variants) {
      retval$p_t_1 <- 1 - pt(retval$t, N - 1) # one-sided!
      retval$t_1_as_chisq1 <- p_to_chisq1(retval$p_t_1)
      retval$p_t_2 <- 2 * retval$p_t_1
      sel <- retval$t < 0
      if (sum(sel) > 0) {
        retval$p_t_2[sel] <- (2 * pt(retval$t, N - 1))[sel]
      }
      retval$t_2_as_chisq1 <- p_to_chisq1(retval$p_t_2)
    }
  })
  # -- fisher (one-sided!) --
  compute_measure(c("fisher", "p_fisher_1"), just_assign = FALSE, expr = {
    retval$p_fisher_1 <- 1 - phyper(a - 1, m, n, k) # attraction
    if (with_variants) {
      retval$fisher_1_as_chisq1 <- p_to_chisq1(retval$p_fisher_1)
      retval$p_fisher_1r <- phyper(a, m, n, k)      # repulsion
      retval$fisher_1r_as_chisq1 <- p_to_chisq1(retval$p_fisher_1r)
    }
    if (p_fisher_2) {
      pf2 <- numeric(length(a))
      for (i in seq_along(a)) {
        m <- matrix(nrow = 2, byrow = TRUE,
                    c(round(a[i]), round(b[i]),
                      round(c[i]), round(d[i])))
        pf2[i] <- fisher.test(m)$p
      }
      retval$p_fisher_2 <- pf2
      if (with_variants) {
        retval$fisher_2_as_chisq1 <- p_to_chisq1(retval$p_fisher_2)
      }
    }
  })
  
  # quick conversion from list to data.frame
  class(retval) <- c("assoc_scores", "data.frame")
  attr(retval, "row.names") <- .set_row_names(length(a))
  rownames(retval) <- types
  # return result
  retval
}

# S3 methods from mclm ========================================
n_types.assoc_scores <- function(x, ...) {
  if (! "assoc_scores" %in% class(x)) {
    stop("argument 'x' must be of the class 'assoc_scores'")
  }
  nrow(x)
}

type_names.assoc_scores <- function(x, ...) {
  if (! "assoc_scores" %in% class(x)) {
    stop("argument 'x' must be of the class 'assoc_scores'")
  }
  rownames(x)
}

explore.assoc_scores <- function(
    x,
    n = 20,
    from = 1,
    from_col = 1,
    perl = TRUE,
    sort_order   = c("none", "G_signed", "PMI", "alpha"),
    use_clear = TRUE,
    ...) {
  # about the argument "from_col":
  #  - initially, the argument "from_col" can contain either a name of a
  #    column or an index; as soon as print() has been called, however,
  #    print_extra$from_col will always contain an index; this is why
  #    all command processing (except for the processing of the command
  #    "i") can safely assume that print_extra$from_col contains an
  #    index.
  
  if (interactive()) {
    ## ---- testing argument from_col --
    if (is.null(from_col) || is.na(from_col[1]) || !is.numeric(from_col)) {
      from_col <- 1
    }
    
    length_x <- n_types(x)                     # n items in x
    ## ---- processing sort_order --
    # testing and processing argument 'sort_order'
    if (is.null(sort_order)  ||
        is.na(sort_order[1])) {
      sort_order <- "none"
    } else {
      sort_order <- sort_order[1]
    }
    if (!sort_order %in% c("none", "alpha") &&
        !sort_order %in% names(x)) {
      sort_order <- "none" # we choose not to send a warning
    }
    # testing and processing sort_order (continued)
    if (length_x > 0) {
      ord <- 1:length_x # applies when sort_order is 'none'
      if (sort_order == "alpha") {
        ord <- order(rownames(x))
      } else if (sort_order %in% c("p_chi2", "p_chi2_Y",
                                   "p_chi2_2T", "p_chi2_2T_Y",
                                   "p_G", "p_G_2T",
                                   "p_t_1", "p_t_2",
                                   "p_fisher_1", "p_fisher_1r")) {
        ord <- order(x[[sort_order]])
      } else if (!sort_order == "none") {
        ord <- order(x[[sort_order]], decreasing = TRUE)
      }  
    }
    
    cur_command <- "i"                         # "idle" (no change of state)
    cur_com_verb <- substr(cur_command, 1, 1)  # actual command 
    cur_regex <- ".*"                          # last regex that was used
    cur_hits <- numeric(0)                     # ids of hits for last regex
    
    # ---- create and initialize printing settings --
    print_extra <- settings()                  
    assign("from_col", from_col, envir = print_extra)
    
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
        cat(mclm_style_dim("l: move one column to the left\n"))        
        cat(mclm_style_dim("r: move one column to the right\n"))        
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
        } else if (cur_com_verb == "l") {  ## u stands for '[l]eft '
          assign("from_col",
                 max(1, print_extra$from_col - 1),
                 envir = print_extra)
        } else if (cur_com_verb == "r") {  ## u stands for '[r]ight'
          assign("from_col",
                 print_extra$from_col + 1,
                 envir = print_extra)
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
            cur_hits <- grep(cur_regex, type_names(x)[ord], perl = perl)
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
        print(x, n = n,
              from = from,
              extra = print_extra,
              sort_order = sort_order,
              ...)
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

# S3 methods from other packages ==============================
as.data.frame.assoc_scores <- function(x, ...) {
  class(x) <- "data.frame"
  df <- cbind(type = rownames(x), x)
  rownames(df) <- NULL
  df
}

as_tibble.assoc_scores <- function(x, ...) {
  as_tibble(as.data.frame(x), ...)
}
# public S3 function print()
print.assoc_scores <- function(
    x,
    n            = 20,
    from         = 1,
    freeze_cols  = NULL, 
    keep_cols    = NULL,
    drop_cols    = NULL,
    from_col     = 1,
    sort_order   = c("none", "G_signed", "PMI", "alpha"),
    extra        = NULL,
    ...) {
  # About the different column selection arguments:
  #   - Argument values in the 'extra' argument take precedence over
  #     the other arguments; for instance, if 'extra$from_col' is not NULL,
  #     then it will overrule the 'from_col' argument.
  #   - The output consist of two areas:
  #       - the 'frozen area' at the left
  #       - the 'regular area' at the right
  #     Both areas are visually separated by a vertocal line (consisting
  #     of pipe symbols.
  #     The distinction between a 'frozen area' and a 'regular area' is
  #     most intuitive if you think of the explore.assoc_scores() function,
  #     which can be used with roughly the same arguments as
  #     print.assoc_scores(): the difference between both areas is that
  #     if in 'exploration' mode you move right or left (with the 'r' and 'l'
  #     commands), then the only columns that move are the ones in the regular
  #     area. In the print.assoc_scores() function, the tool for moving
  #     left or right is the 'from_col' command (see below).
  #   - The names of the types are always displayed and are always the first
  #     column of the 'frozen' area. None of the function arguments can change
  #     this. Likewise, the indices of the items are always displayed.
  #   - The 'freeze_cols' argument determines which other columns are displayed
  #     in the frozen area. If freeze_cols is NULL, then the default setting
  #     applies, which means that the columns "a", "PMI", and "G_signed", are
  #     displayed in the "frozen area" (at least, if they are present in x).
  #     If you want no measures in the "frozen area", then specify
  #     freeze_cols = NA or freeze_cols = character(0).
  #   - The keep_cols and drop_cols arguments jointly determine which measures
  #     are displayed in the 'regular area'. [Notice that keep_cols and
  #     drop_cols have no effect on the 'frozen area'.]
  #   - 'keep_cols' is stronger than 'drop_cols': for instance, if
  #     a column name is present in both 'keep_cols' and 'drop_cols', then
  #     it will be displayed in the 'regular area' (at least, if it is present
  #     in x and if moreover it fits on the screen).
  #   - 'from_col' identifies the first column in the regular area to show;
  #     if it is a number, then it is an index among the columns that 
  #     are selected for display in the 'regular area'. If from_col points
  #     to  another column than the first one, then anything before that
  #     column is not displayed. 
  
  # testing and processing argument 'x'
  if (!"assoc_scores" %in% class(x)) {
    stop("x must be of the class 'assoc_scores'")
  }
  n_items <- nrow(x)
  # testing argument 'extra'
  if (!is.null(extra) && !is.environment(extra)) {
    stop("incorrect use of the argument 'extra'")
  }
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
  from <- max(0, round(from))
  # adjusting 'n' to 'from'
  n <- max(0, min(n, n_items - from + 1))
  # testing and processing argument 'sort_order'
  if (is.null(sort_order)  ||
      is.na(sort_order[1])) {
    sort_order <- "none"
  } else {
    sort_order <- sort_order[1]
  }
  if (!sort_order %in% c("none", "alpha") &&
      !sort_order %in% names(x)) {
    sort_order <- "none" # we choose not to send a warning
  }
  # testing and processing sort_order (continued)
  if (n > 0) {
    idx <- from:(from + n - 1)
    ord <- idx # applies when sort_order is 'none'
    if (sort_order == "alpha") {
      ord <- order(rownames(x))[idx]
    } else if (sort_order %in% c("p_chi2", "p_chi2_Y",
                                 "p_chi2_2T", "p_chi2_2T_Y",
                                 "p_G", "p_G_2T",
                                 "p_t_1", "p_t_2",
                                 "p_fisher_1", "p_fisher_1r")) {
      ord <- order(x[[sort_order]])[idx]
    } else if (!sort_order == "none") {
      ord <- order(x[[sort_order]], decreasing = TRUE)[idx]
    }  
  }
  sel_cols <- names(x) # all cols are selected at this point
  # testing and processing argument freeze_cols
  if (is.null(freeze_cols)) {
    freeze_cols <- c("a", "PMI", "G_signed")
  }
  freeze_cols <- intersect(freeze_cols, sel_cols)
  nofreeze_cols <- setdiff(sel_cols, freeze_cols)  
  # testing and processing argument keep_cols
  if (!is.null(keep_cols) && !is.na(keep_cols[1])) {
    drop_cols <- NULL # keep_cols takes precedence
    sel_cols <- intersect(keep_cols, sel_cols)
    freeze_cols <- intersect(freeze_cols, sel_cols)
  }  
  # testing and processing argument drop_cols
  if (!is.null(drop_cols) && !is.na(drop_cols[1])) {
    sel_cols <- setdiff(sel_cols, drop_cols)  
  }
  # ---- testing and processing from_col --
  if (!is.null(extra$from_col)) {
    from_col <- extra$from_col  # info in 'extra', if present, takes precedence 
  }    
  if (is.character(from_col) && !is.na(from_col[1])) {
    from_col <- match(from_col[1], nofreeze_cols)
  }
  if (is.null(from_col) || is.na(from_col[1])) {
    from_col <- 1
  }
  from_col <- min(from_col, length(nofreeze_cols))
  nofreeze_cols <- nofreeze_cols[from_col:length(nofreeze_cols)]
  sel_cols <- c(freeze_cols, nofreeze_cols)
  n_frozen <- length(freeze_cols)
  # ---- printing 'x' --
  cat(mclm_style_dim(paste0(
    "Association scores (types in list: ",
    n_items)))
  if (sort_order != "none") {
    cat(mclm_style_dim(paste0(
      ", sort order criterion: ",
      sort_order)))
  }
  cat(mclm_style_dim(paste0(
    ")\n")))
  # --
  if (n > 0) {
    types <- rownames(x)[ord]
    form_cols <- vector(mode = "list",
                        length = length(sel_cols))
    for (j in seq_along(sel_cols)) {
      form_cols[[j]] <- format(c(sel_cols[j], 
                                 format(round(x[[sel_cols[j]]][ord], 3),
                                        scientify = FALSE, 
                                        justify = "right")), 
                               justify = "right")
    }
    nchar_types <- nchar(types)
    if (!is.null(extra$type_regex)) {
      types <- show_matches(types, extra$type_regex)
    }    
    format_types <- mclm_pad_left(
      c("type", types),
      max(nchar("type"), nchar_types),
      nchar_x = c(nchar("type"), nchar_types))
    #
    #format_types <- stringi::stri_pad_left( # don't use format() (unicode !)
    #                  c("type", types),
    #                  max(nchar("type"), nchar(types)))
    format_idx <- format(c("", 
                           format(idx,
                                  scientify = FALSE, 
                                  justify = "right")), 
                         justify = "right")
    # -- determine number of cols that can be printed
    n_printed_col <- 0
    c_avail <- (getOption("width") - 7 -   # 7 to account for scroll bar
                  nchar(format_idx[1]) - 1 -  # to account for space between cols
                  nchar(format_types[1]) - 1) # result can be negative
    width_next <- nchar(form_cols[[n_printed_col + 1]][1]) + 1 
    while (!is.null(width_next) && width_next < c_avail) {
      n_printed_col <- n_printed_col + 1
      c_avail <- c_avail - width_next 
      if (n_printed_col == length(form_cols)) {
        width_next <- NULL
      } else {
        width_next <- nchar(form_cols[[n_printed_col + 1]][1]) + 1
      }
    }
    # -- print titles
    cat(format_idx[1])
    cat(" ", format_types[1], sep = "")
    for (j in 1:n_printed_col) {
      if (j == (n_frozen + 1)) {
        cat(mclm_style_very_dim("|"))
        if (from_col > 1) {
          cat(mclm_style_very_dim("_"))
        }
      } else {
        cat(" ")
      }
      cat(form_cols[[j]][1], sep = "")
    }
    cat("\n")
    # -- optionally print dots
    if (from > 1) cat(mclm_style_very_dim("...\n"))
    # -- print items  
    for (i in 2:length(format_idx)) {
      cat(mclm_style_very_dim(format_idx[i]))
      cat(" ", format_types[i], sep = "")
      for (j in 1:n_printed_col) {
        if (j == (n_frozen + 1)) {
          cat(mclm_style_very_dim("|"))
          if (from_col > 1) {
            cat(mclm_style_very_dim("_"))
          }
        } else {
          cat(" ")
        }
        cat(form_cols[[j]][i])
      }
      cat("\n")
    }
    # -- optionally print dots
    if ((from + n - 1) < n_items) cat(mclm_style_very_dim("...\n"))
    # -- optionally indicate that not all columns are printed
    if (length(sel_cols) > n_printed_col) {
      cat(mclm_style_dim(paste0(
        "<number of extra columns to the right: ",
        length(sel_cols) - n_printed_col,
        ">\n")))
    }
  }
  # --- update information in 'extra' --
  # This is important for the case where the calling function, e.g. explore(),
  # needs this updated information.
  
  if (is.environment(extra)) {
    assign("from_col", from_col, envir = extra)
  }
  
  invisible(x)
}

# Utility functions ============================================================

zero_plus <- function(x, small_pos = 0.00001) {
  # auxiliary function that makes all values in numeric vector x strictly positive
  # small.pos stands for 'small positive constant'
  x[x <= 0] <- small_pos
  x
}

p_to_chisq1 <- function(p) {
  # returns the 'p right quantile' in the chi-square distribution with one df
  return(qchisq(1 - p, 1))
}

chisq1_to_p <- function(x) {
  # returns the proportion of the chi-square distribution with one df
  # that sits to the right of x
  1 - pchisq(x, 1)
}

# Public functions applied to assoc_scores =====================================

# write an assoc_scores object
write_assoc <- function(x,
                        file = "",
                        sep = "\t",
                        file_encoding = "UTF-8") {
  if (nrow(x) > 0) {
    lines <- rownames(x)
    for (i in 1:ncol(x)) {
      lines <- paste(lines, as.character(x[, i]), sep = sep)
    }
    names <- paste(c("type", names(x)), collapse = sep)
    x <- paste0(paste(append(names, lines), collapse = "\n"), "\n")
    con <- file(file, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)
  }
  invisible(x)
}

# read association scores from file
read_assoc <- function(file,
                       header = TRUE,
                       sep = "\t",
                       quote = "",
                       comment_char = "",
                       file_encoding = "UTF-8",
                       ...) {
  lines <- read_txt(file, file_encoding = file_encoding) 
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
      col_name <- cols[i]
      d[[col_name]] <- unlist(lapply(cells, "[", i))
      d[[col_name]] <- type.convert(d[[col_name]], as.is = TRUE)
  }
  rownames(d) <- d$type
  d$type <- NULL
  class(d) <- c("assoc_scores", class(d))
  d
}

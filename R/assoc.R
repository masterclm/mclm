# TODO implement sort function
# TODO check as.data.frame
# TODO do a sort as the last step in assoc_scores
# IDEA maybe add citations in the class definition? (For the measures...)

# All functions in this section assume the data stem from frequency tables of 
# the form:
#                           target item  other item
#         target context              a           b
#          other context              c           d 
#
# Moreover all functions accept a,b,c,d to be equal sized vectors of length
# 1 or higher. They take a[1],b[1],c[1] and d[1] to stem from frequency table 1, 
# a[2],b[2],c[2] and d[2], to stem from frequency table 2, etc.

# Create cooc_info =============================================================

#' Constructor of class 'cooc_info'
#'
#' @param target_freqlist,ref_freqlist An object of class [`freqlist`].
#'
#' @return A named list of two [`freqlist`] objects, "target_freqlist"
#'   and "ref_freqlist", of class `cooc_info`.
#' @noRd
cooc_info <- function(target_freqlist,
                      ref_freqlist) {
  retval <- list(target_freqlist = target_freqlist,
                 ref_freqlist = ref_freqlist)
  class(retval) = "cooc_info"
  retval
}

# CHANGED merged documentation of surf_cooc and text_cooc together (not ontly option).
## in fact the details of "text_cooc" described "surf_cooc"
# NOTE Old documentation assumed that there was an as_text argument, which is not correct.
# TODO add examples to the following documentation
# REVIEW document class itself here?

#' Build collocation frequencies.
#' 
#' These functions builds a surface or textual collocation frequency for a specific node.
#' 
#' Two major steps can be distinguished in the procedure conducted by these functions.
#' The first major step is the *identification of the (sequence of) tokens* that,
#' for the purpose of this analysis, will be considered to be the content of the corpus.
#' 
#' The function arguments that jointly determine the details of this step are
#' `re_drop_line`, `line_glue`, `re_cut_area`, `re_token_splitter`,
#' `re_token_extractor`, `re_drop_token`, `re_token_transf_in`,
#' `token_transf_out`, and `token_to_lower`.
#' The sequence of tokens that is the ultimate outcome of this step is then
#' handed over to the second major step of the procedure.
#' 
#' The second major step is the *establishment of the co-occurrence frequencies*.
#' The function arguments that jointly determine the details of this step are
#' `re_node` and `re_boundary` for both functions,
#' and `w_left` and `w_right` for `surf_cooc()` only.
#' It is important to know that this second step is conducted after the tokens
#' of the corpus have been identified, and that it is applied to a sequence of
#' tokens, not to the original text. More specifically the regular expressions
#' `re_node` and `re_boundary` are tested against individual tokens,
#' as they are identified by the token identification procedure.
#' Moreover, in `surf_cooc()`, the numbers `w_left` and `w_right`
#' also apply to tokens a they are identified by the token identification procedure.
#'
#' @param x List of filenames of the corpus files.
#' @param re_node Regular expression used for identifying instances of the 'node',
#'   i.e. the target item for which collocation information is collected.
#' @param w_left Number of tokens to the left of the 'node' that are treated as
#'   belonging to the co-text of the 'node'. (But also see `re_boundary`.)
#' @param w_right Number of tokens to the right of the 'node' that are treated as
#'   belonging to the co-text of the 'node'. (But also see `re_boundary`.)
#' @param re_boundary Regular expression.
#'   
#'   For `text_cooc()`, it identifies boundaries between 'textual units'.
#'   
#'   For `surf_cooc()`, it identifies 'cut-off' points for the co-text of
#'   the 'node'. If it is not `NULL`, the maximum length of the left and right
#'   co-texts are still given by `w_left` and `w_right`, but if a match
#'   for `re_boundary` is found within the co-text, both the 'boundary token'
#'   and all tokens beyond it are excluded.
#' @param re_drop_line Regular expression or `NULL`. if `NULL`, the
#'   argument  is ignored. Otherwise, lines in the corpus that match it are
#'   treated as not belonging to the corpus and excluded from the results.
#' @param line_glue Character vector or `NULL`. if `NULL`, the argument
#'   is ignored.
#'   Otherwise, all the lines in the corpus are glued together in one character
#'   vector of length 1, with the string `line_glue` pasted in between
#'   consecutive lines.
#'   
#'   This value can also be equal to an empty string `""`.
#'   
#'   The 'line glue' operation is conducted immediately after the 'drop line' operation.
#' @param re_cut_area Regular expression or `NULL`. if `NULL`, the
#'   argument  is ignored.
#'   Otherwise, all matches in the corpus are 'cut out' from the text
#'   prior to the identification of the tokens and are therefore not taken into
#'   account when identifying the tokens.
#'   
#'   The 'cut area' operation is conducted immediately after the 'line glue' operation.
#' @param re_token_splitter Regular expression or `NULL`. if `NULL`,
#'   the argument is ignored and `re_token_extractor` is used instead.
#'   Otherwise, it identifies the areas between the tokens within a line of the corpus.
#'   
#'   The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_token_extractor Regular expression that identifies the locations of
#'   the actual tokens. It is only used if `re_token_splitter` is `NULL`.
#'   Currently the implementation of this argument is a lot less time-efficient
#'   than that of `re_token_splitter`.
#'   
#'   The 'token identification' operation is conducted immediately after the
#'   'cut area' operation.
#' @param re_drop_token Regular expression or `NULL`. if `NULL`, the
#'   argument is ignored. Otherwise, it identifies tokens to be excluded from the results.
#'   
#'   The 'drop token' operation is conducted immediately after the 'token
#'   identification' operation.
#' @param re_token_transf_in A regular expression that identifies areas in the
#'   tokens that are to be transformed. This argument works together with
#'   `token_transf_out`. If either of them is `NULL`, they are both ignored.
#'   
#'   Otherwise, all matches in the tokens for `re_token_transf_in` are
#'   replaced with the replacement string `token_transf_out`.
#'   
#'   The 'token transformation' operation is conducted immediately after the
#'   'drop token' transformation.
#' @param token_transf_out A 'replacement string'. This argument works together
#'   with `re_token_transf_in` and is ignored if either argument is `NULL`.
#' @param token_to_lower Boolean value. Whether tokens should be converted to
#'   lowercase before returning the results.
#'   
#'   The 'token to lower' operation is conducted immediately after the 'token
#'   transformation' operation.
#' @param perl Boolean value. Whether the PCRE flavor of regular expressions
#'   should be used in the arguments that contain regular expressions.
#' @param blocksize Number indicating how many corpus files are read to memory
#'   'at each individual step' during the steps in the procedure. Normally the
#'   default value of `300` should not be changed, but when one works with
#'   exceptionally small corpus files, it may be worthwhile to use a higher
#'   number, and when one works with exceptionally large corpus files, it may be
#'   worthwhile to use a lower number.
#' @param verbose Boolean value. If `TRUE`, messages are pritned to the
#'   console to indicate progress.
#' @param dot_blocksize Boolean value. If `TRUE`, dots are printed to the
#'   console to indicate progress.
#' @param file_encoding Encoding of the input files.
#'   
#'   Either a character vector of length 1, in which case all files are assumed
#'   to be in the same encoding, or a character vector with the same length as
#'   `x`, which allows for different encodings for different files.
#'
#' @return An object of class `cooc_info`, containing information on
#'   co-occurrence frequencies.
#' @name create_cooc
NULL

#' @describeIn create_cooc Build surface collocation frequencies
#' @export
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

#' @describeIn create_cooc Build textual collocation frequencies
#' @export
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

# REVIEW describe class here?
#' Association scores used in collocation analysis and keyword analysis
#' 
#' `assoc_scores` and `assoc_abcd` take as their arguments co-occurrence
#' frequencies of a number of items and return a range of association scores used
#' in collocation analysis, collostruction analysis and keyword analysis.
#' 
#' @details 
#' ## Input and output
#' [assoc_scores()] takes as its arguments a target frequency list and a reference
#' frequency lists (either as two [`freqlist`] objects or as a
#' [`cooc_info`][create_cooc()] object) and returns a number of popular measures
#' expressing, for (almost) every item in either one of these lists, the extent
#' to which the item is attracted to the target context, when compared to the
#' reference context. The "almost" is added between parentheses because, with
#' the default settings, some items are automatically excluded from the output
#' (see `min_freq`).
#' 
#' [assoc_abcd()] takes as its arguments four vectors `a`, `b`, `c`, and `d`, of
#' equal length. Each tuple of values `(a[i], b[i], c[i], d[i])`, with `i` some
#' integer number between 1 and the length of the vectors, is assumed to represent
#' the four numbers *a*, *b*, *c*, *d* in a contingency table of the type:
#' 
#' |                 | **tested item** | **any other item** | **total** |
#' |----------------:|----------------:|-------------------:|----------:|
#' |  target context | *a*             | *b*                | *m*       |
#' |reference context| *c*             | *d*                | *n*       |
#' |total            | *k*             | *l*                | *N*       |
#' 
#' In the above table *m*, *n*, *k*, *l* and *N* are marginal frequencies.
#' More specifically, *m = a + b*, *n = c + d*, *k = a + c*, *l = b + d* and *N = m + n*.
#' 
#' ## Dealing with zeros
#' 
#' Several of the association measures break down when one or more of the values
#' `a`, `b`, `c`, and `d` are zero (for instance, because this would lead to
#' division by zero or taking the log of zero). This can be dealt with in different
#' ways, such as the Haldane-Anscombe correction.
#' 
#' Strictly speaking, Haldane-Anscombe correction specifically applies to the
#' context of (log) odds ratios for two-by-two tables and boils down to adding
#' `0.5` to each of the four values `a`, `b`, `c`, and `d`
#' in every two-by-two contingency table for which the original values
#' `a`, `b`, `c`, and `d` would not allow us to calculate
#' the (log) odds ratio, which happens when one (or more than one) of the four
#' cells is zero.
#' Using the Haldane-Anscombe correction, the (log) odds ratio is then calculated
#' on the bases of these 'corrected' values for `a`, `b`, `c`, and `d`.
#' 
#' However, because other measures that do not compute (log) odds ratios might
#' also break down when some value is zero, all measures will be computed on the
#' 'corrected' contingency matrix.
#'  
#' If the `haldane` argument is set to `FALSE`, division by zero or taking the
#' log of zero is avoided by systematically adding a small positive value to all
#' zero values for `a`, `b`, `c`, and `d`. The argument `small_pos`
#' determines which small positive value is added in such cases. Its default value is `0.00001`.
#'
#' @param x Either an object of class [`freqlist`]
#'   or an object of class [`cooc_info`][create_cooc()].
#'   
#'   If `x` is a [`freqlist`], it is interpreted as the target frequency
#'   list (i.e. the list with the frequency of items in the target context) and
#'   `y` must be a [`freqlist`] with the frequency of items in the
#'   reference context.
#'   
#'   If `x` is an object of class [`cooc_info`][create_cooc()] instead, it is interpreted
#'   as containting target frequency information, reference frequency information
#'   and corpus size information.
#' @param y An object of class [`freqlist`] with the frequencies of the
#'   reference context if `x` is also a [`freqlist`]. If `x` is an
#'   object of class [`cooc_info`][create_cooc()], this argument is ignored.
#' @param a Numeric vector expressing how many times some tested item
#'   occurs in the target context.
#'   More specifically, `a[[i]]`, with `i` an integer, expresses
#'   how many times the `i`-th tested item occurs in the target context.
#' @param b Numeric vector expressing how many times other items than the tested
#'   item occur in the target context.
#'   More specifically, `b[[i]]`, with `i` an integer, expresses
#'   how many times *other* items than the `i`-th tested item
#'   occur in the target context.
#' @param c Numeric vector expressing how many times some tested
#'   item occurs in the reference context.
#'   More specifically, `c[[i]]`, with `i` an integer, expresses
#'   how many times the `i`-th tested item occurs in the reference context.
#' @param d Numeric vector expressing how many times items other than the tested
#'   item occur in the reference context.
#'   More specifically, `d[[i]]`, with `i` an integer, expresses
#'   how many times *other* items than the `i`-th tested item occur
#'   in the reference context.
#' @param types A character vector containing the names of the linguistic items
#'   of which the association scores are to be calculated, or `NULL`. If
#'   `NULL`, [assoc_abcd()] creates dummy types such as `"t001"`,
#'   `"t002"`, etc.
#' @param min_freq Minimum value for `a[[i]]` (or for the frequency of an
#'   item in the target frequency list) needed for its corresponding item to be
#'   included in the output.
#' @param measures Character vector containing the association measures (or related
#'   quantities) for which scores are requested. Supported measure names (and
#'   related quantities) are described in `Value` below.
#'   
#'   If `measures` is `NULL`, it is interpreted as short for the default selection,
#'   i.e. `c("exp_a", "DP_rows", "RR_rows", "OR", "MS", "Dice", "PMI",
#'   "chi2_signed", "G_signed", "t", "fisher")`.
#'   
#'   If `measures` is `"ALL"`, all supported measures are calculated (but not
#'   necessarily all the variants; see `with_variants`).
#' @param with_variants Boolean. Whether, for the requested `measures`, all
#'   variants should be included in the output (`TRUE`) or only the main
#'   version (`FALSE`). See also `p_fisher_2`.
#' @param show_dots Boolean. Whether a dot should be shown in console each time
#'   calculations for a measure are finished.
#' @param p_fisher_2 Boolean, only relevant if `"fisher"` is included in
#'   `measures`. If `TRUE`, the p-value for a two-sided test (testing
#'   for either attraction or repulsion) is also calculated. By default, only
#'   the (computationally less demanding) p-value for a one-sided test is
#'   calculated. See `Value` for more details.
#' @param haldane Boolean value. Should the Haldane-Anscombe correction be used?
#'   (See the Details section.)
#'   
#'   If `haldane` is `TRUE`, and there is at least one zero frequency
#'   in a contingency table, the correction is used for all measures calculated
#'   for that table, not just for measures that need this to be done.
#' @param small_pos Alternative (but sometimes inferior) approach to dealing with
#'   zero frequencies, compared to `haldane`. The argument `small_pos`
#'   only applies when `haldane` is set to `FALSE`.
#'   (See the Details section.)
#'      
#'   If `haldane` is `FALSE`, and there is at least one zero frequency
#'   in a contingency table, adding small positive values to the zero frequency
#'   cells is done systematically for all measures calculated for that table,
#'   not just for measures that need this to be done.
#'
#' @return An object of class `assoc_scores`. This is a kind of data frame with
#'   as its rows all items from either the target frequency list or the reference
#'   frequency list with a frequency larger than `min_freq` in the target list,
#'   and as its columns a range of measures that express the extent to which
#'   the items are attracted to the target context (when compared to the reference
#'   context).
#'   Some columns don't contain actual measures but rather additional information
#'   that is useful for interpreting other measures.
#'   
#'   ## Possible columns
#'   
#'   The following sections describe the (possible) columns in the output. All
#'   of these measures are reported if `measures` is set to `"ALL"`. Alternatively,
#'   each measure can be requested by specifying its name in a character vector
#'   given to the `measures` argument. Exceptions are described in the sections
#'   below.
#'   
#'   ### Observed and expected frequencies
#'   
#'   - `a`, `b`, `c`, `d`: The frequencies in cells *a*, *b*, *c* and *d*,
#'   respectively. If one of them is `0`, they will be augmented by 0.5 or `small_pos`
#'   (see `Details`). These output columns are always present.
#'   - `dir`: The direction of the association: `1` in case of relative attraction
#'   between the tested item and the target context (if \eqn{\frac{a}{m} \ge \frac{c}{n}}) and
#'   `-1` in case of relative repulsion between the target item and the target
#'   context (if \eqn{\frac{a}{m} < {c}{n}}).
#'   - `exp_a`, `exp_b`, `exp_c`, `exp_d`: The expected values for cells *a*, *b*,
#'   *c* and *d*, respectively. All these columns will be included if `"expected"`
#'   is in `measures`. `exp_a` is also one of the default measures and is therefore included
#'   if `measures` is `NULL`. The values of these columns are computed as follows:
#'       + `exp_a` = \eqn{\frac{m \times k}{N}}
#'       + `exp_b` = \eqn{\frac{m \times l}{N}}
#'       + `exp_c` = \eqn{\frac{n \times k}{N}}
#'       + `exp_d` = \eqn{\frac{n \times l}{N}}
#'       
#'   ### Effect size measures
#'   
#'   Some of these measures are based on proportions and can therefore be
#'   computed either on the rows or on the columns of the contingency table. Each
#'   measure can be requested on its own, but pairs of measures can also be
#'   requested with the first part of their name, as indicated in their corresponding
#'   descriptions.
#'   
#'   - `DP_rows` and `DP_cols`: The difference of proportions, sometimes also
#'   called Delta-p (\eqn{\Delta p}), between rows and columns respectively.
#'   Both columns are present if `"DP"` is included in `measures`. `DP_rows`
#'   is also included if `measures` is `NULL`.
#'   They are calculated as follows:
#'       + `DP_rows` = \eqn{\frac{a}{m} - \frac{c}{n}}
#'       + `DP_cols` = \eqn{\frac{a}{k} - \frac{b}{l}}
#'   - `perc_DIFF_rows` and `perc_DIFF_cols`: These measures can be seen as
#'   normalized versions of Delta-p, i.e. essentially the same measures divided
#'   by the denominator and multiplied by `100`. They therefore express how large
#'   the difference of proportions is, relative to the reference proportion.
#'   The multiplication by `100` turns the resulting 'relative difference of
#'   proportion' into a percentage.
#'   Both columns are present if `"perc_DIFF"` is included in `measures`.
#'   They are calculated as follows:
#'       + `perc_DIFF_rows` = \eqn{100 * \frac{(a / m) - (c / n)}{c / n}}
#'       + `perc_DIFF_cols` = \eqn{100 * \frac{(a / k) - (b / l)}{c / n}}
#'   - `DC_rows` and `DC_cols`: The difference coefficient can be seen as a
#'   normalized version of Delta-p, i.e. essentially dividing the difference of
#'   proportions by the sum of proportions.
#'   Both columns are present if `"DC"` is included in `measures`.
#'   They are calculated as follows:
#'       + `DC_rows` = \eqn{\frac{(a / m) - (c / n)}{(a / m) + (c / n)}}
#'       + `DC_cols` = \eqn{\frac{(a / k) - (b / l)}{(a / k) + (b / l)}}
#'   - `RR_rows` and `RR_cols`: Relative risk for the rows and columns
#'   respectively. `RR_rows` represents then how large the proportion in the
#'   target context is, relative to the proportion in the reference context.
#'   Both columns are present if `"RR"` is included in `measures`.
#'   `RR_rows` is also included if `measures` is `NULL`.
#'   They are calculated as follows:
#'       + `RR_rows` = \eqn{\frac{a / m}{c / n}}
#'       + `RR_cols` = \eqn{\frac{a / k}{b / l}}
#'   - `LR_rows` and `LR_cols`: The so-called 'log ratio' of the rows and
#'   columns, respectively. It can be seen as a transformed version of the relative
#'   risk, viz. its binary log.
#'   Both columns are present if `"LR"` is included in `measures`.
#'   They are calculated as follows:
#'      + `LR_rows` = \eqn{\log_2\left(\frac{a / m}{c / n}\right)}
#'       + `LR_cols` = \eqn{\log_2\left(\frac{a / k}{b / l}\right)}
#'   
#'   Other measures use the contingency table in a different way and therefore
#'   don't have a complementary row/column pair. In order to retrieve these columns,
#'   if `measures` is not `"ALL"`, their name must be in the `measures` vector.
#'   Some of them are included by default, i.e. if `measures` is `NULL`.
#'       
#'   - `OR`: The odds ratio, which can be calculated either as
#'   \eqn{\frac{a/b}{c/d}} or as \eqn{\frac{a/c}{b/d}}.
#'   This column is present `measures` is `NULL`.
#'   - `log_OR`: The log odds ratio, which can be calculated either as
#'   \eqn{\log\left(\frac{a/b}{c/d}\right)} or as \eqn{\log\left(\frac{a/c}{b/d}\right)}.
#'   In other words, it is the natural log of the odds ratio.
#'   - `MS`: The minimum sensitivity, which is calculated as
#'   \eqn{\min(\frac{a}{m}, \frac{a}{k})}.
#'   In other words, it is either \eqn{\frac{a}{m}} or \eqn{\frac{a}{k}}, whichever is lowest.
#'   This column is present `measures` is `NULL`.
#'   - `Jaccard`: The Jaccard index, which iscalculated as
#'   \eqn{\frac{a}{a + b + c}}. It expresses *a*, which is the frequency of the
#'   test item in the target context, relative to *b + c + d*, i.e. the frequency
#'   of all other contexts.
#'   - `Dice`: The Dice coefficient, which is calculated as
#'   \eqn{\frac{2a}{m + k}}. It expresses the harmonic mean of \eqn{\frac{a}{m}} and \eqn{\frac{a}{k}}
#'   This column is present `measures` is `NULL`.
#'   - `logDice`: An adapted version of the Dice coefficient. It is calculated as
#'   \eqn{14 + \log_2\left(\frac{2a}{m + k}\right)}. In other words, it is `14`
#'   plus the binary log of the Dice coefficient.
#'   - `phi`: The phi coefficient (\eqn{\phi}), which is calculated as
#'   \eqn{\frac{(a \times d) - (b \times c)}{ \sqrt{m \times n \times k \times l}}}.
#'   - `Q`: Yule's Q, which is calculated as
#'   \eqn{\frac{(a \times d) - (b \times c)}{(a \times d)(b \times c)}}.
#'   - `mu`: The measure mu (\eqn{\mu}), which is calculated as
#'   \eqn{\frac{a}{\mathrm{exp\_a}}} (see `exp_a`).
#'   - `PMI` and `pos_PMI`: (Positive) pointwise mutual information,
#'   which can be seen as a modification of the mu measure and is calculated as
#'   \eqn{\log_2\left(\frac{a}{\mathrm{exp\_a}}\right)}. In `pos_PMI`, negative
#'   values are set to `0`.
#'   The `PMI` column is present `measures` is `NULL`.
#'   - `PMI2` and `PMI3`: Modified versions of `PMI` that aim to give relatively
#'   more weight to cases with relatively higher *a*. However, because of this
#'   modification, they are not pure effect size measures any more.
#'       + `PMI2` = \eqn{\log_2\left(\frac{a^2}{\mathrm{exp\_a}}\right)}
#'       + `PMI3` = \eqn{\log_2\left(\frac{a^3}{\mathrm{exp\_a}}\right)}
#'       
#'   ### Strength of evidence measures
#'   
#'   The first measures in this section tend to come in triples: a test statistic,
#'   its p-value (preceded by `p_`) and its signed version (followed by `_signed`).
#'   The test statistics indicate evidence of either attraction or repulsion.
#'   Thus, in order to indicate the direction of the relationship, a negative
#'   sign is added in the "signed" version when \eqn{\frac{a}{k} < \frac{c}{l}}.
#'   
#'   In each of these cases, the name of the main measure (e.g. `"chi2"`)
#'   and/or its signed counterpart (e.g. `"chi2_signed"`) must be in the `measures`
#'   argument, or `measures` must be `"ALL"`, for the columns to be included in
#'   the output. If the main function is requested, the signed counterpart will
#'   also be included, but if only the signed counterpart is requested, the non-signed
#'   version will be excluded.
#'   For the p-value to be retrieved, either the main measure or its signed version
#'   must be requested and, *additionally*, the `with_variants` argument must be
#'   set to `TRUE`.
#'   
#'   - `chi2`, `p_chi2` and `chi2_signed`: The chi-squared test statistic
#'   (\eqn{\chi^2}) as used in a chi-squared test of independence or in a
#'   chi-squared test of homogeneity for a two-by-two contingency table.
#'   Scores of this measure are high when there is strong evidence for attraction,
#'   but also when there is strong evidence for repulsion.
#'   The `chi2_signed` column is present if `measures` is `NULL`.
#'   `chi2` is calculated as follows: \deqn{
#'                         \frac{(a-\mathrm{exp\_a})^2}{\mathrm{exp\_a}} +
#'                         \frac{(b-\mathrm{exp\_b})^2}{\mathrm{exp\_b}} +
#'                         \frac{(c-\mathrm{exp\_c})^2}{\mathrm{exp\_c}} +
#'                         \frac{(d-\mathrm{exp\_d})^2}{\mathrm{exp\_d}}
#'                        }.
#'   - `chi2_Y`, `p_chi2_Y` and `chi2_Y_signed`: The chi-squared test statistic
#'   (\eqn{\chi^2}) as used in a chi-squared test with Yates correction
#'   for a two-by-two contingency table.
#'  `chi2_Y` is calculated as follows: \deqn{
#'                         \frac{(|a-\mathrm{exp\_a}| - 0.5)^2}{\mathrm{exp\_a}} +
#'                         \frac{(|b-\mathrm{exp\_b}| - 0.5)^2}{\mathrm{exp\_b}} +
#'                         \frac{(|c-\mathrm{exp\_c}| - 0.5)^2}{\mathrm{exp\_c}} +
#'                         \frac{(|d-\mathrm{exp\_d}| - 0.5)^2}{\mathrm{exp\_d}}
#'                        }.
#'   - `chi2_2T`, `p_chi2_2T` and `chi2_2T_signed`: The chi-squared test statistic
#'   (\eqn{\chi^2}) as used in a chi-squared goodness-of-fit test applied to the
#'   first column of the contingency table. The `"2T"` in the name stands for
#'   'two terms' (as opposed to `chi2`, which is soemtimes the 'four terms' version).
#'   `chi2_2T` is calculated as follows: \deqn{
#'                         \frac{(a-\mathrm{exp\_a})^2}{\mathrm{exp\_a}} +
#'                         \frac{(c-\mathrm{exp\_c})^2}{\mathrm{exp\_c}}
#'                        }.
#'   - `chi2_2T_Y`, `p_chi2_2T_Y` and `chi2_2T_Y_signed`: The chi-squared test statistic
#'   (\eqn{\chi^2}) as used in a chi-squared goodness-of-fit test with Yates correction, applied to the
#'   first column of the contingency table.
#'   `chi2_2T_Y` is calculated as follows: \deqn{
#'                           \frac{(|a-\mathrm{exp\_a}| - 0.5)^2}{\mathrm{exp\_a}} +
#'                           \frac{(|c-\mathrm{exp\_c}| - 0.5)^2}{\mathrm{exp\_c}}
#'                          }.
#'   - `G`, `p_G` and `G_signed`: G test statistic, which is also sometimes
#'   called log-likelihood ratio (LLR) and, somewhat confusingly, G-squared.
#'   This is the test statistic as used in a log-likelihood ratio test for independence
#'   or homogeneity in a two-by-two contingency table.
#'   Scores are high in case of strong evidence for attraction, but also in case
#'   of strong evidence of repulsion.
#'   The `G_signed` column is present if `measures` is `NULL`.
#'   `G` is calcualted as follows: \deqn{
#'                   2 \left(
#'                   a \times \log(\frac{a}{\mathrm{exp\_a}}) +
#'                   b \times \log(\frac{b}{\mathrm{exp\_b}}) +
#'                   c \times \log(\frac{c}{\mathrm{exp\_c}}) +
#'                   d \times \log(\frac{d}{\mathrm{exp\_d}})
#'                   \right)
#'                  }
#'   - `G_2T`, `p_G_2T` and `G_2T_signed`: The test statistic
#'   used in a log-likelihood ratio test for goodness-of-fit applied to the first
#'   column of the contingency table.
#'   The `"2T"` stands for 'two terms'.
#'   `G_2T` is calculated as follows: \deqn{
#'                   2 \left(
#'                   a \times \log(\frac{a}{\mathrm{exp\_a}}) +
#'                   c \times \log(\frac{c}{\mathrm{exp\_c}})
#'                   \right)
#'                  }
#'                  
#'   The final two groups of measures take a different shape. The
#'   `_as_chisq1` columns compute `qchisq(1 - p, 1)`, with `p` being the p-values
#'   they are transforming, i.e. the `p` right quantile in a \eqn{\chi^2}
#'   distribution with one degree of freedom (see [p_to_chisq1()]).
#'   
#'   - `t`, `p_t_1`, `t_1_as_chisq1`, `p_t_2` and `t_2_as_chisq1`:
#'   The t-test statistic, used for a t-test for the proportion \eqn{\frac{a}{N}}
#'   in which the null hypothesis is based on \eqn{\frac{k}{N}\times\frac{m}{N}}.
#'   Column `t` is present if `"t"` is included in `measures` or if `measures` is
#'   `"ALL"` or `NULL`. The other four columns are present if `t` is requested and if,
#'   additionally, `with_variants` is `TRUE`.
#'       + `t` = \eqn{
#'                    \frac{
#'                    a/N + k/N + m/N
#'                    }{
#'                    \sqrt{((a/N)\times (1-a/N))/N}
#'                    }
#'                     }
#'       + `p_t_1` is the p-value that corresponds to `t` when assuming a one-tailed
#'       test that only looks at attraction; `t_1_as_chisq1` is its transformation.
#'       + `p_t_2` is the p-value that corresponds to `t` when assuming a two-tailed
#'       test, viz. that looks at both attraction and repulsion; `t_2_as_chisq1` is
#'       its transformation.
#'  - `p_fisher_1`, `fisher_1_as_chisq1`, `p_fisher_1r`, `fisher_1r_as_chisq1`:
#'  The p-value of a one-sided Fisher exact test.
#'  The column `p_fisher_1` is present if either `"fisher"` or `"p_fisher"` are in `measures`
#'  or if `measures` is `"ALL"` or `NULL`. The other columns are present if `p_fisher_1` as
#'  been requested and if, additionally, `with_variants` is `TRUE`.
#'    
#'      + `p_fisher_1` and `p_fisher_1r` are the p-values of the Fisher exact test
#'    that look at attraction and repulsion respectively.
#'    
#'      + `fisher_1_as_chisq1` and `fisher_1r_as_chisq1` are their respective transformations..
#'  - `p_fisher_2` and `fisher_2_as_chisq1`: p-value for a two-sided Fisher
#'  exact test, viz. looking at both attraction and repulsion. `p_fisher_2`
#'  returns the p-value and `fisher_2_as_chisq1` is its transformation.
#'  The `p_fisher_2` column is present if either `"fisher"` or `"p_fisher_1"` are
#'  in `measures` or if `measures` is `"ALL"` or `NULL` and if, additionally, `p_fisher_2` is
#'  `TRUE`. `fisher_2_as_chisq1` is present if `p_fisher_2` was requested and,
#'  additionally, `with_variants` is `TRUE`.
#'  
#'  ## Properties of the class
#'  
#'  An object of class `assoc_scores` has:
#'  - associated [as.data.frame()], [print()][print.assoc_scores()] and [tibble::as_tibble()]
#'  methods,
#'  - an interactive [explore()] method and useful getters, viz. [n_types()] and
#'  [type_names()].
#'  
#'  An object of this class can be saved to file with [write_assoc()] and read
#'  with [read_assoc()].
#'   
#' @name assoc_scores
#' @export
#' @examples 
#' assoc_abcd(10 , 200, 100,  300, types = "four")
#' assoc_abcd(30, 1000,  14, 5000, types = "fictitious")
#' assoc_abcd(15, 5000,  16, 1000, types = "toy")
#' assoc_abcd( 1,  300,   4, 6000, types = "examples")
#' 
#' a <- c(10,    30,    15,    1)
#' b <- c(200, 1000,  5000,  300)
#' c <- c(100,   14,    16,    4)
#' d <- c(300, 5000, 10000, 6000)
#' types <- c("four", "fictitious", "toy", "examples")
#' (scores <- assoc_abcd(a, b, c, d, types = types))
#' 
#' print(scores, sort_order = "PMI")
#' print(scores, sort_order = "alpha")
#' print(scores, sort_order = "none")
#' print(scores, sort_order = "nonsense")
#' 
#' print(scores, sort_order = "PMI",
#'       keep_cols = c("a", "exp_a", "PMI", "G_signed"))
#' print(scores, sort_order = "PMI",
#'       keep_cols = c("a", "b", "c", "d", "exp_a", "G_signed"))
#' print(scores, sort_order = "PMI",
#'      drop_cols = c("a", "b", "c", "d", "exp_a", "G_signed",
#'                     "RR_rows", "chi2_signed", "t"))
NULL

#' @rdname assoc_scores
#' @export
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

#' @rdname assoc_scores
#' @export
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

#' @rdname n_types
#' @exportS3Method n_types assoc_scores
#' @export
n_types.assoc_scores <- function(x, ...) {
  if (! "assoc_scores" %in% class(x)) {
    stop("argument 'x' must be of the class 'assoc_scores'")
  }
  nrow(x)
}

#' @rdname type_names
#' @exportS3Method type_names assoc_scores
#' @export
type_names.assoc_scores <- function(x, ...) {
  if (! "assoc_scores" %in% class(x)) {
    stop("argument 'x' must be of the class 'assoc_scores'")
  }
  rownames(x)
}

#' @rdname explore
#' @exportS3Method explore assoc_scores
#' @export
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

#' @exportS3Method as.data.frame assoc_scores
#' @export
as.data.frame.assoc_scores <- function(x, ...) {
  class(x) <- "data.frame"
  df <- cbind(type = rownames(x), x)
  rownames(df) <- NULL
  df
}

#' @exportS3Method tibble::as_tibble assoc_scores
#' @export
as_tibble.assoc_scores <- function(x, ...) {
  as_tibble(as.data.frame(x), ...)
}

#' @rdname mclm_print
#' @exportS3Method print assoc_scores
#' @export
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

#' Make all values strictly higher than zero
#' 
#' This is an auxiliary function that makes all values in numeric vector x strictly
#' positive by replacing all values equal to or lower than zero with
#' the values in `small.pos`. `small_pos` stands for 'small positive constant'.
#' 
#' @param x A numeric vector.
#' @param small_pos A (small) positive number to replace negative values and 0s.
#'
#' @return A copy of `x` in which all values equal to or lower than zero are
#'   replaced by `small_pos`.
#' @export
#'
#' @examples
#' (x <- rnorm(30))
#' zero_plus(x)
zero_plus <- function(x, small_pos = 0.00001) {
  x[x <= 0] <- small_pos
  x
}

#' P right quantile in chi-squared distribution with 1 df
#' 
#' P right quantile that takes as its argument a probability `p` and that returns
#' the `p` *right quantile* in the \eqn{\chi^2} distribution with one degree of 
#' freedom. In other words, it returns a value *q* such that a proportion `p`
#' \eqn{\chi^2} distribution with one degree of freedom lies above *q*.
#'
#' @param p A proportion.
#'
#' @return The `p` *right quantile* in the \eqn{\chi^2} distribution with
#'   one degree of freedom.
#' @export
#' @seealso [chisq1_to_p()]
p_to_chisq1 <- function(p) {
  # returns the 'p right quantile' in the chi-square distribution with one df
  return(qchisq(1 - p, 1))
}

#' Proportion of chi-squared distribution with 1 df that sits to the right of x
#' Helper function that takes as its argument a numerical value `x` and
#' that returns the proportion *p* of the chi-squared
#' distribution with one degree of freedom that sits to the right of the
#' value `x.
#'
#' @param x A number.
#'
#' @return The proportion *p* of the chi-squared distribution with one
#' degree of freedom that sits to the right of the value `x`.
#' @export
#' @seealso [p_to_chisq1()]
chisq1_to_p <- function(x) {
  1 - pchisq(x, 1)
}

# Public functions applied to assoc_scores =====================================

#' Write association scores to file
#' 
#' This function writes an object of class [`assoc_scores`] to a file.
#'
#' @param x An object of class [`assoc_scores`].
#' @param file Name of the output file.
#' @param sep Field separator for the output file.
#' @param file_encoding Encoding for the output file.
#'
#' @return Invisibly, `x`.
#' @seealso [read_assoc()]
#' @export
#'
#' @examples
#' \dontrun{
#' txt1 <- "we're just two lost souls swimming in a fish bowl,
#' year after year, running over the same old ground,
#' what have we found? the same old fears.
#' wish you were here."
#' flist1 <- freqlist(txt1, as_text = TRUE)
#' txt2 <- "picture yourself in a boat on a river
#' with tangerine dreams and marmelade skies
#' somebody calls you, you answer quite slowly
#' a girl with kaleidoscope eyes"
#' flist2 <- freqlist(txt2, as_text = TRUE)
#' (scores <- assoc_scores(flist1, flist2, min_freq = 0))
#' write_assoc(scores, "example_scores.tab")
#' (scores2 <- read_assoc("example_scores.tab"))
#' }
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

# CHANGED removed arguments that are not used at all, namely header, quote and comment_char
#' Read association scores from file
#' 
#' This function reads a file written by [write_assoc()].
#'
#' @param file Path of the input file.
#' @param sep Field separator in the input file.
#' @param file_encoding Encoding of the input file.
#' @param ... Additional arguments.
#'
#' @return An object of class [`assoc_scores`].
#' @export
#' @seealso [write_assoc()]
#'
#' @inherit write_assoc examples
read_assoc <- function(file,
                       sep = "\t",
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

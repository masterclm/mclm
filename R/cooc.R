# build a coocurrence matrix   
#cooc_mat_char <- function(x,
#                          left = 5,
#                          right = 5,
#                          re_boundary = NULL,
#                          item_names = NULL,
#                          feature_names = NULL,
#                          re_drop_line = NULL,
#                          line_glue = NULL, 
#                          re_cut_area = NULL,
#                          re_token_splitter = re("[^_\\p{L}\\p{N}\\p{M}'-]+"),
#                          re_token_extractor = re("[_\\p{L}\\p{N}\\p{M}'-]+"),
#                          re_drop_token = NULL,
#                          re_token_transf_in = NULL,
#                          token_transf_out = NULL,
#                          token_to_lower = TRUE,
#                          perl = TRUE) {
#  if (!"tokens" %in% class(x)) {
#    x < -    tokenize(x,
#                      re_drop_line = re_drop_line,
#                      line_glue = line_glue,
#                      re_cut_area = re_cut_area,
#                      re_token_splitter = re_token_splitter,
#                      re_token_extractor = re_token_extractor,
#                      re_drop_token = re_drop_token,
#                      re_token_transf_in = re_token_transf_in,
#                      token_transf_out = token_transf_out,
#                      token_to_lower = token_to_lower,
#                      perl = perl,
#                      ngram_size = ngram_size,
#                      max_skip = max_skip,
#                      ngram_sep = ngram_sep,
#                      ngram_n_open = ngram_n_open,
#                      ngram_open = ngram_open)
#  }
#  boundaries <- vector(mode = "integer", length = 0)
#  if (!is.null(re_boundary)) {
#    boundaries <- grep(re_boundary, x, perl = perl)
#  }
#  if (is.null(item_names)) {
#    item_names <- types(x)
#  }
#  if (is.null(featue_names)) {
#    feature_names <- item_names
#  }
#  cooc_mat_char2(x, left, right, item_names, feature_names, boundaries)
#}
#
#cooc_mat_char2 <- function(x,
#                           left,
#                           right,
#                           item_names,
#                           feature_names,
#                           boundaries) {
#  
#}
#

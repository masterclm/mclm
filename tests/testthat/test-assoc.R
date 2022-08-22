# TODO test re_token_splitter, re_token_extractor, token_to_lower, blocksize
test_that("surf_cooc works properly", {
  toy_file <- get_fnames(test_path("minicorpus"))
  
  surf <- surf_cooc(toy_file, "corpus")
  expect_s3_class(surf, "cooc_info")
  expect_length(surf, 2)
  expect_s3_class(surf$target_freqlist, "freqlist")
  expect_s3_class(surf$ref_freqlist, "freqlist")
  
  expect_equal(n_tokens(surf$target_freqlist), 29)
  expect_length(surf$target_freqlist, 22)
  expect_equal(surf$target_freqlist[["a"]], 4)
  expect_equal(surf$target_freqlist[["the"]], 1)
  
  expect_equal(n_tokens(surf$ref_freqlist), 12)
  expect_length(surf$ref_freqlist, 11)
  expect_equal(surf$ref_freqlist[["was"]], 2)
  
  expect_false("corpus" %in% type_names(surf$target_freqlist))
  expect_false("corpus" %in% type_names(surf$ref_freqlist))
  
  # not found node
  no_node <- surf_cooc(toy_file, "missing")
  expect_s3_class(no_node, "cooc_info")
  expect_length(no_node, 2)
  expect_length(no_node$target_freqlist, 0)
  expect_length(no_node$ref_freqlist, 31)
  
  # longer window
  long <- surf_cooc(toy_file, "corpus", w_left = 5, w_right = 10)
  expect_length(long$target_freqlist, 27)
  expect_length(long$ref_freqlist, 4)
  expect_false("corpus" %in% type_names(long$target_freqlist))
  expect_equal(long$target_freqlist[["was"]], 3)
  
  # re boundary
  a_bound <- surf_cooc(toy_file, "corpus", re_boundary = "\\ba\\b")
  expect_length(a_bound$target_freqlist, 19)
  expect_length(a_bound$ref_freqlist, 12)
  expect_false("a" %in% type_names(a_bound$target_freqlist))
  expect_false("a" %in% type_names(a_bound$ref_freqlist))
  expect_true("want" %in% type_names(a_bound$ref_freqlist))
  
  # re_cut_area
  no_vowels <- surf_cooc(toy_file, "crps",
                         re_cut_area = "[aeiou]")
  expect_length(no_vowels$target_freqlist, 21)
  expect_true("tny" %in% type_names(no_vowels$target_freqlist))
  expect_false("crps" %in% type_names(no_vowels$target_freqlist))
  expect_false("tiny" %in% type_names(no_vowels$target_freqlist))
  
  # re_drop_token
  no_a <- surf_cooc(toy_file, "corpus", re_drop_token = "a")
  expect_length(no_a$target_freqlist, 17)
  expect_length(no_a$ref_freqlist, 4)
  expect_false("habemus" %in% type_names(no_a$target_freqlist))
  expect_false("corpus" %in% type_names(no_a$target_freqlist))
  expect_false("a" %in% type_names(no_a$ref_freqlist))
  
  # transformation
  transf <- surf_cooc(toy_file, "c-rp-s",
                      re_token_transf_in = "([aeiou])",
                      token_transf_out = "-")
  expect_length(transf$target_freqlist, 22)
  expect_length(transf$ref_freqlist, 11)
  expect_false("tiny" %in% type_names(transf$target_freqlist))
  expect_true("t-ny" %in% type_names(transf$target_freqlist))
  expect_false("c-rp-s" %in% type_names(transf$target_freqlist))
  expect_true("-" %in% type_names(transf$ref_freqlist))
  
})

test_that("text_cooc works properly", {
  toy_file <- get_fnames(test_path("minicorpus"))
  
  tc <- text_cooc(toy_file, "tiny")
  expect_s3_class(tc, "cooc_info")
  expect_length(tc, 2)
  expect_s3_class(tc$target_freqlist, "freqlist")
  expect_s3_class(tc$ref_freqlist, "freqlist")
  
  expect_equal(n_tokens(tc$target_freqlist), 25)
  expect_length(tc$target_freqlist, 23)
  expect_equal(tc$target_freqlist[["a"]], 2)
  expect_equal(tc$target_freqlist[["corpus"]], 2)
  
  expect_equal(n_tokens(tc$ref_freqlist), 10)
  expect_length(tc$ref_freqlist, 10)
  expect_equal(tc$ref_freqlist[["corpus"]], 1)
  
  expect_false("tiny" %in% type_names(tc$target_freqlist))
  expect_false("tiny" %in% type_names(tc$ref_freqlist))
  
  # not found node
  no_node <- text_cooc(toy_file, "missing")
  expect_s3_class(no_node, "cooc_info")
  expect_length(no_node, 2)
  expect_length(no_node$target_freqlist, 0)
  expect_length(no_node$ref_freqlist, 31)
  
  # re boundary
  a_bound <- text_cooc(toy_file, "tiny", re_boundary = "\\ba\\b")
  expect_length(a_bound$target_freqlist, 9)
  expect_length(a_bound$ref_freqlist, 22)
  expect_false("a" %in% type_names(a_bound$target_freqlist))
  expect_false("a" %in% type_names(a_bound$ref_freqlist))
  expect_true("file" %in% type_names(a_bound$ref_freqlist))
  
  # re_cut_area
  no_vowels <- text_cooc(toy_file, "tny",
                         re_cut_area = "[aeiou]")
  expect_length(no_vowels$target_freqlist, 21)
  expect_false("tny" %in% type_names(no_vowels$target_freqlist))
  expect_true("crps" %in% type_names(no_vowels$target_freqlist))
  expect_false("tiny" %in% type_names(no_vowels$target_freqlist))
  
  # re_drop_token
  no_a <- text_cooc(toy_file, "tiny", re_drop_token = "a")
  expect_length(no_a$target_freqlist, 15)
  expect_length(no_a$ref_freqlist, 8)
  expect_false("habemus" %in% type_names(no_a$target_freqlist))
  expect_false("tiny" %in% type_names(no_a$target_freqlist))
  expect_false("a" %in% type_names(no_a$ref_freqlist))
  
  # transformation
  transf <- text_cooc(toy_file, "t-ny",
                      re_token_transf_in = "[aeiou]",
                      token_transf_out = "-")
  expect_length(transf$target_freqlist, 23)
  expect_length(transf$ref_freqlist, 10)
  expect_false("tiny" %in% type_names(transf$target_freqlist))
  expect_false("t-ny" %in% type_names(transf$target_freqlist))
  expect_true("c-rp-s" %in% type_names(transf$target_freqlist))
  expect_true("-" %in% type_names(transf$ref_freqlist))
  
})

# TODO test variants, all measures?
# TODO test haldane & small_pos
test_that("assoc_scores computes properly", {
  toy_file <- get_fnames(test_path("minicorpus"))
  surf <- surf_cooc(toy_file, "corpus")
  scores <- assoc_scores(surf, min_freq = 2)
  
  expect_s3_class(scores, "assoc_scores")
  expect_length(scores, 16)
  expect_equal(nrow(scores), 4)
  
  expect_equal(n_types(scores), 4)
  expect_match(type_names(scores)[[1]], "a")
  
  # check values of first row
  a <- scores["a",]
  expect_equal(a$a, 4)
  expect_equal(a$b, 25)
  expect_equal(a$c, 1)
  expect_equal(a$d, 11)
  expect_equal(a$dir, 1)
  expect_equal(a$exp_a, 3.536, tolerance = 0.001)
  expect_equal(a$DP_rows, 0.055, tolerance = 0.01)
  expect_equal(a$RR_rows, 1.655, tolerance = 0.01)
  expect_equal(a$OR, 1.76, tolerance = 0.01)
  expect_equal(a$MS, 0.138, tolerance = 0.01)
  expect_equal(a$Dice, 0.235, tolerance = 0.01)
  expect_equal(a$PMI, 0.178, tolerance = 0.01)
  expect_equal(a$chi2_signed, 0.236, tolerance = 0.01)
  expect_equal(a$G_signed, 0.252, tolerance = 0.01)
  expect_equal(a$t, 0.244, tolerance = 0.01)
  expect_equal(a$p_fisher_1, 0.539, tolerance = 0.01)
  
})

# Basic methods

test_that("scores dataframes work", {
  toy_file <- get_fnames(test_path("minicorpus"))
  surf <- surf_cooc(toy_file, "corpus")
  scores <- assoc_scores(surf, min_freq = 2)
  
  as_df <- as.data.frame(scores)
  expect_s3_class(as_df, "data.frame")
  expect_equal(nrow(as_df), 4)
  expect_length(as_df, 17)
  
  expect_s3_class(as_tibble(scores), "tbl_df")
})

test_that("scores are printed properly", {
  toy_file <- get_fnames(test_path("minicorpus"))
  surf <- surf_cooc(toy_file, "corpus")
  scores <- assoc_scores(surf, min_freq = 2)
  
  expect_output(print(scores), "Association scores")
  expect_output(print(scores), "types in list: 4")
  expect_output(print(scores), "type")
  expect_output(print(scores, keep_cols = "b"), "b")
  
  expect_error(print(scores, n = numeric()))
  expect_error(print(scores, n = NA))
  expect_error(print(scores, from = numeric()))
})

test_that("scores are sorted properly", {
  toy_file <- get_fnames(test_path("minicorpus"))
  surf <- surf_cooc(toy_file, "corpus")
  scores <- assoc_scores(surf, min_freq = 2)
  
  expect_s3_class(sort(scores), "assoc_scores")
  expect_equal(sort(scores), sort(scores, sort_order = "name"))
  expect_match(type_names(sort(scores))[[1]], "a")
  expect_match(type_names(sort(scores))[[2]], "tiny")
  expect_match(type_names(sort(scores, sort_order = "PMI"))[[1]], "tiny")
  expect_match(type_names(sort(scores, FALSE, sort_order = "PMI"))[[1]], "you")
  expect_match(type_names(sort(scores, FALSE, sort_order = "alpha"))[[2]], "do")
  by_t <- sort(scores, sort_order = "p_fisher_1")
  expect_true(by_t$p_fisher_1[[1]] == min(by_t$p_fisher_1))
})

# Helpers

test_that("Helpers do what they should", {
  expect_true(min(zero_plus(c(-1, 2, 0))) > 0)
  expect_true(sum(zero_plus(c(-1, 2, 0))) > 2)
  expect_equal(sum(zero_plus(c(-1, 2, 0), 0.1)), 2.2)
  
  expect_equal(p_to_chisq1(0.2), qchisq(1 - 0.2, 1))
  expect_equal(chisq1_to_p(0.2), 1 - pchisq(0.2, 1))
})

# QUESTION is it possible to test an interactive function like explore()?
# ngrams have their own tests
# TODO test perl = FALSE

toy_corpus <- "That ice-cream was extra-delicious, wasn't it?"
lines_corpus <- "
--start--
This
corpus
has
one
word
per
line
--end--
"
basic_tks <- tokenize(toy_corpus)
words_tks <- tokenize(toy_corpus, re_token_splitter = "\\W+")

test_that("tokens are properly created", {
  expect_s3_class(words_tks, "tokens")
  expect_s3_class(words_tks, "character")
  
  expect_length(words_tks, 9)
  expect_length(words_tks[], 9)
  expect_length(basic_tks, 6)
  expect_length(tokenize(2), 1)
  lsc <- tokenize(lines_corpus)
  expect_length(lsc, 9)
  expect_match(lsc[1], "--start--")
  
  # re_drop_line
  expect_length(tokenize(toy_corpus, re_drop_line = "was"), 0)
  expect_length(tokenize(lines_corpus, re_drop_line = "corpus"), 8)
  
  # re_cut_area
  cut_tks <- tokenize(lines_corpus, re_cut_area = "--[^-]+--")
  expect_length(cut_tks, 7)
  expect_match(cut_tks[1], 'this')
  
  # token_extractor
  ext <- tokenize(lines_corpus, re_token_splitter = NULL,
                  re_token_extractor = "^[a-z]{1,3}$")
  expect_length(ext, 3)
  expect_match(ext[1], "has")
  
  # transformation
  expect_match(
    tokenize(toy_corpus, re_token_transf_in = "-", token_transf_out = "~")[2],
    "ice~cream")
  expect_match(
    tokenize(toy_corpus, re_token_transf_in = "([tc])", token_transf_out = "\\1\\1")[2],
    "icce-ccream")
  expect_match(
    tokenize(toy_corpus, re_token_transf_in = NULL, token_transf_out = "\\1\\1")[2],
    "ice-cream")
  
  # to lower
  expect_match(
    tokenize(toy_corpus, token_to_lower = FALSE)[1],
    "That"
  )
  
  # ngrams
  expect_match(
    tokenize(toy_corpus, ngram_size = 2)[1],
    "that_ice-cream"
  )
  expect_match(
    tokenize(toy_corpus, re_token_splitter = "\\W+", ngram_size = 2)[1],
    "that_ice"
  )
  expect_match(
    tokenize(toy_corpus, re_token_transf_in = "([tc])",
             token_transf_out = "\\1\\1", ngram_size = 2,
             token_to_lower = FALSE)[1],
    "Thatt_icce-ccream")
  expect_match(
    tokenize(toy_corpus, ngram_size = 3, ngram_n_open = 1)[1],
    "that_\\[]_was"
  )
  expect_match(
    tokenize(toy_corpus, ngram_size = 3, ngram_n_open = 1,
             re_token_splitter = "\\W+")[1],
    "that_\\[]_cream"
  )
  
  # empty data
  expect_length(tokenize(NULL), 0)
  expect_length(tokenize(character()), 0)
  
  # errors
  expect_error(tokenize(toy_corpus, ngram_size = "2"))
  expect_error(tokenize(toy_corpus, ngram_size = 1, ngram_sep = 2))
})

test_that("tokens are printed properly", {
  expect_output(print(words_tks), "Token sequence")
  expect_output(print(words_tks), "length 9")
  expect_output(print(basic_tks), "length 6")
  expect_output(print(words_tks, n = 3), "length 9")
  expect_output(print(words_tks), "token")
  expect_output(print(words_tks), "idx")
  expect_output(print(words_tks), "cream")
  
  # Errors and warnings
  expect_error(print(words_tks, n = numeric()))
  expect_error(print(words_tks, n = NA))
  expect_error(print(words_tks, n = "1"))
  expect_warning(print(words_tks, n = c(5, 20)))
  
  expect_warning(print(words_tks, from = c(1, 3)))
  expect_error(print(words_tks, from = "1"))
  
  # TODO test extra argument?
})

test_that("dataframe conversion of tokens works", {
  expect_length(as.data.frame(words_tks), 1)
  expect_s3_class(as.data.frame(words_tks), "data.frame")
  expect_equal(nrow(as.data.frame(words_tks)), 9)
  
  expect_s3_class(as_tibble(words_tks), "tbl_df")
  expect_equal(nrow(as_tibble(words_tks)), 9)
})

test_that("tokens sorting works", {
  expect_length(sort(words_tks), 9)
  expect_s3_class(sort(words_tks), "tokens")
  
  expect_match(sort(words_tks, decreasing = TRUE)[1], "wasn")
  expect_match(sort(basic_tks)[1], "extra-delicious")
})

test_that("tokens summary works", {
  sum_tks <- summary(words_tks)
  expect_s3_class(sum_tks, "summary.tokens")
  expect_length(sum_tks, 1)
  expect_equal(sum_tks$n_tokens, 9)
  expect_output(print(sum_tks), "Token sequence")
  expect_output(print(sum_tks), "length 9")
})

# Basic mclm methods ====

test_that("as_tokens works", {
  expect_s3_class(as_tokens(c("one", "two")), "tokens")
})

test_that("n_tokens works for tokens", {
  expect_equal(n_tokens(words_tks), 9)
  expect_equal(n_tokens(basic_tks), 6)
})

test_that("n_types works for tokens", {
  expect_equal(n_types(words_tks), 9)
  expect_equal(n_types(tokenize("type one type two type three")), 4)
})

test_that("trunc_at works", {
  no_keep <- trunc_at(words_tks, re("was"))
  expect_s3_class(no_keep, "tokens")
  expect_length(no_keep, 3)
  expect_match(no_keep[3], "cream")
  
  keep <- trunc_at(words_tks, re("was"), keep_this = TRUE)
  expect_length(keep, 4)
  expect_match(keep[4], "was")
  
  last <- trunc_at(words_tks, re("was"), last_match = TRUE)
  expect_length(last, 6)
  expect_match(last[6], "delicious")
  expect_match(
    trunc_at(words_tks, re("was"), last_match = TRUE, keep_this = TRUE)[7],
    "wasn"
  )
  
  from_end <- trunc_at(words_tks, re("was"), from_end = TRUE)
  expect_length(from_end, 2)
  expect_match(from_end[2], "it")
  
  expect_error(trunc_at(words_tks))
  expect_error(trunc_at(words, "was"))
})

## Subsetting methods ====
test_that("subsetting tokens by position works", {
  expect_s3_class(keep_pos(words_tks, 1:3), "tokens")
  expect_s3_class(drop_pos(words_tks, 1:3), "tokens")
  
  expect_length(keep_pos(words_tks, 1:5), 5)
  expect_length(keep_pos(words_tks, 3), 1)
  expect_match(keep_pos(words_tks, 3), "cream")
  expect_length(drop_pos(words_tks, 1:5), length(words_tks)-5)
  expect_length(words_tks[1:5], 5)
  expect_length(words_tks[1:5, invert = TRUE], length(words_tks)-5)
  expect_match(words_tks[3], "cream")
  
  # negative indices
  expect_equal(drop_pos(words_tks, 3), keep_pos(words_tks, -3))
  expect_equal(keep_pos(words_tks, -3), keep_pos(words_tks, 3, invert = TRUE))
  
  # indices beyond length
  expect_length(keep_pos(words_tks, 20), 0)
  expect_length(keep_pos(words_tks, 5:15), 5)
})

test_that("subsetting tokens by type works", {
  matcher <- c("ice", "cream", "was")
  full_match <- keep_types(words_tks, matcher)
  
  expect_s3_class(full_match, "tokens")
  expect_s3_class(drop_types(words_tks, matcher), "tokens")
  
  expect_length(full_match, 3)
  expect_length(drop_types(words_tks, matcher), length(words_tks)-3)
  expect_length(keep_types(words_tks, c(matcher, 'test')), 3)
  expect_length(keep_types(words_tks, 1), 0)
  
  expect_length(words_tks['cream'], 1)
  expect_length(words_tks['test'], 0)
  
  expect_equal(drop_types(words_tks, matcher),
               keep_types(words_tks, matcher, invert = TRUE))
})

test_that("subsetting tokens by regex works", {
  # TODO add tests for the PERL argument
  t_regex <- "^t"
  dots_regex <- "^....?$"
  t_match <- keep_re(words_tks, t_regex)
  dots_match <- keep_re(words_tks, dots_regex)
  dots_unmatch <- drop_re(words_tks, dots_regex)
  dots_unmatch2 <- keep_re(words_tks, dots_regex, invert = TRUE)
  
  expect_s3_class(t_match, "tokens")
  expect_s3_class(dots_unmatch, "tokens")
  
  expect_length(t_match, 2)
  expect_length(dots_match, 4)
  expect_length(dots_unmatch, 5)
  expect_length(dots_unmatch2, 5)
  expect_equal(dots_unmatch, dots_unmatch2)
  expect_warning(
    double_pattern <- keep_re(words_tks, c(dots_regex, t_regex))
  )
  expect_equal(double_pattern, dots_match)
  expect_equal(keep_re(words_tks, re(t_regex)), t_match)
  
  expect_match(t_match[1], "that")
  expect_match(dots_match[3], "was")
  expect_match(dots_unmatch[2], "extra")
  
  expect_length(keep_re(words_tks, "test"), 0)
  expect_length(drop_re(words_tks, "test"), length(words_tks))
  
  expect_equal(words_tks[re(t_regex)], t_match)
  expect_length(words_tks[re("^.$")], 1)
})

test_that("subsetting tokens with logic works", {
  alternated <- keep_bool(words_tks, c(TRUE, FALSE))
  expect_s3_class(alternated, "tokens")
  expect_s3_class(drop_bool(words_tks, c(TRUE, FALSE)), "tokens")
  
  expect_length(alternated, 5)
  by_nchar <- nchar(words_tks) < 5
  expect_length(keep_bool(words_tks, by_nchar), sum(by_nchar))
  expect_length(drop_bool(words_tks, by_nchar), length(words_tks)-sum(by_nchar))
  
  expect_length(words_tks[c(TRUE, FALSE)], 5)
  expect_length(words_tks[by_nchar], sum(by_nchar))
  expect_equal(keep_bool(words_tks, TRUE), drop_bool(words_tks, FALSE))
  expect_equal(drop_bool(words_tks, by_nchar), keep_bool(words_tks, by_nchar, invert = TRUE))
})

test_that("subsetting methods throw errors", {
  expect_error(keep_pos(words_tks, "1"))
  expect_error(keep_pos(words_tks, 1, invert = NULL))
  expect_error(keep_pos(words_tks, 1, invert = "TRUE"))
  expect_error(keep_pos(words_tks, c(1, -1)))
  
  expect_error(keep_types(words_tks, "after", invert = NULL))
  expect_error(keep_types(words_tks, "after", invert = "TRUE"))
  
  expect_error(keep_re(words_tks, 1))
  expect_error(keep_re(words_tks, NA))
  expect_error(keep_re(words_tks, "test", perl = "TRUE"))
  expect_error(keep_re(words_tks, "test", invert = "TRUE"))
  expect_warning(keep_re(words_tks, "test", perl = c(TRUE, FALSE)))
  expect_warning(keep_re(words_tks, "test", invert = c(TRUE, FALSE)))
  
  expect_error(keep_bool(words_tks, 1))
  expect_error(keep_bool(words_tks, "TRUE"))
  expect_error(keep_bool(words_tks, NULL))
  expect_error(keep_bool(words_tks, c(TRUE, NA)))
  expect_error(keep_bool(words_tks, c(TRUE, FALSE), invert = "TRUE"))
  expect_warning(keep_bool(words_tks, c(TRUE, FALSE), invert = c(TRUE, FALSE)))
  
  expect_error(words_tks[c(1, -1)])
  expect_error(words_tks[list('a', 'b')])
  
  expect_error(drop_pos(words_tks, invert = TRUE))
  expect_error(drop_types(words_tks, invert = TRUE))
  expect_error(drop_re(words_tks, invert = TRUE))
  expect_error(drop_bool(words_tks, invert = TRUE))
})

# Merging and other functions ====

test_that("merging works for tokens", {
  merge1 <- tokens_merge(words_tks, basic_tks)
  expect_s3_class(merge1, "tokens")
  expect_length(merge1, 15)
  expect_equal(n_types(merge1), 12)
  expect_match(merge1[13], "extra-delicious")
  
  merge2 <- tokens_merge_all(words_tks, basic_tks, basic_tks)
  expect_s3_class(merge2, "tokens")
  expect_length(merge2, 21)
  expect_equal(n_types(merge2), 12)
  expect_match(merge2[19], "extra-delicious")
  
  expect_error(tokens_merge(words_tks, toy_corpus))
  expect_error(tokens_merge(words_tks, basic_tks, basic_tks))
  expect_error(tokens_merge_all(words_tks, basic_tks, toy_corpus))
})

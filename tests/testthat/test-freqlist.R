# Arguments that come from tokenize are tested with that class
# QUESTION is it possible to test an interactive function like explore()?
# QUESTION should we try to test as_text = FALSE?

toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentences. And it lived happily ever after."

flist <- freqlist(toy_corpus, as_text = TRUE)
subset_flist <- flist[c('once', 'lived', 'corpus')]
# General methods ====

test_that("freqlists are properly created", {
  expect_s3_class(flist, "freqlist")
  expect_s3_class(flist, "table")
  expect_length(flist, 19)
  expect_length(flist[], 19)
})

test_that("freqlists are printed properly", {
  expect_output(print(flist), "Frequency list")
  expect_output(print(flist), "types in list: 19")
  expect_output(print(flist), "tokens in list: 21")
  expect_output(print(flist, n = 3), "types in list: 19")
  expect_output(print(flist), "rank")
  expect_output(print(flist), "after")
  
  # Errors and warnings
  expect_error(print(flist, n = numeric()))
  expect_error(print(flist, n = NA))
  expect_error(print(flist, n = "1"))
  expect_warning(print(flist, n = c(5, 20)))
  
  expect_warning(print(flist, from = c(1, 3)))
  expect_error(print(flist, from = "1"))
  
  # TODO test extra argument?
})

test_that("dataframe conversion of freqlists works", {
  expect_length(as.data.frame(flist), 4)
  expect_s3_class(as.data.frame(flist), "data.frame")
  expect_equal(nrow(as.data.frame(flist)), 19)
  expect_equal(colnames(as.data.frame(flist)),
               c("rank", "type", "abs_freq", "nrm_freq"))
  
  expect_s3_class(as_tibble(flist), "tbl_df")
  expect_equal(nrow(as_tibble(flist)), 19)
})

test_that("freqlist sorting works", {
  expect_length(sort(flist), 19)
  expect_s3_class(sort(flist), "freqlist")
  
  dec_sort <- sort(flist, decreasing = TRUE)
  expect_equal(as_numeric(dec_sort[1]), 1)
  expect_equal(type_names(dec_sort[1]), 'was')
  
  alpha_sort <- sort(flist, TRUE, sort_crit = "names")
  expect_equal(as_numeric(alpha_sort[2]), 1)
  expect_equal(type_names(alpha_sort[2]), 'upon')
  
  expect_equal(sort(subset_flist, sort_crit = "ranks"), sort(subset_flist, sort_crit = "orig_ranks"))
  expect_equal(type_names(sort(subset_flist, sort_crit = "orig_ranks")[1]), "corpus")
  expect_equal(type_names(sort(subset_flist, sort_crit = "names")[2]), "lived")
  
  expect_equal(sort(flist), sort(flist, TRUE, "freqs"))
  
  expect_error(sort(flist, NULL))
  expect_error(sort(flist, NA))
  expect_error(sort(flist, "TRUE"))
  expect_error(sort(flist, sort_crit = NULL))
  expect_error(sort(flist, sort_crit = "alpha"))
  expect_warning(sort(flist, na.last = TRUE))
})

test_that("freqlist summary works", {
  sum_flist <- summary(flist)
  expect_s3_class(sum_flist, "summary.freqlist")
  expect_length(sum_flist, 3)
  expect_equal(sum_flist$n_types, 19)
  expect_equal(sum_flist$n_tokens, 21)
  expect_equal(sum_flist$tot_n_tokens, 21)
  expect_output(print(sum_flist), "Frequency list")
  expect_output(print(sum_flist), "types in list: 19")
  expect_output(print(sum_flist), "tokens in list: 21")
  
  sum_subset <- summary(subset_flist)
  expect_equal(sum_subset$n_types, 3)
  expect_equal(sum_subset$n_tokens, 3)
  expect_equal(sum_subset$tot_n_tokens, 21)
  expect_output(print(sum_subset), "Frequency list")
  expect_output(print(sum_subset), "types in list: 3")
  expect_output(print(sum_subset), "tokens in list: 3")
  expect_output(print(sum_subset), "total number of tokens: 21")
})

# Basic mclm methods ====

test_that("as_freqlist() works", {
  expect_s3_class(as_freqlist(c("a" = 12, "toy" = 53, "example" = 20)), "freqlist")
  with_tnt <- as_freqlist(c("a" = 12, "toy" = 53, "example" = 20), tot_n_tokens = 1300)
  expect_equal(tot_n_tokens(with_tnt), 1300)
  
  expect_s3_class(as_freqlist(table(c("one", "two", "one"))), "freqlist")
  expect_error(as_freqlist(c("one", "two", "one")))
})

test_that("n_tokens works for freqlists", {
  expect_equal(n_tokens(flist), 21)
  expect_equal(n_tokens(subset_flist), 3)
})

test_that("n_types works for freqlists", {
  expect_equal(n_types(flist), 19)
  expect_equal(n_types(subset_flist), 3)
})

test_that("type_names works for freqlists", {
  expect_match(type_names(flist)[5], "consisted")
  expect_length(type_names(flist), 19)
})

test_that("tot_n_tokens works", {
  expect_equal(tot_n_tokens(flist), 21)
  expect_equal(tot_n_tokens(subset_flist), 21)
  subset2 <- subset_flist
  tot_n_tokens(subset2) <- 50
  expect_equal(tot_n_tokens(subset2), 50)
  
  expect_error(tot_n_tokens(subset2) <- NULL)
  expect_error(tot_n_tokens(subset2) <- numeric(0))
  expect_warning(tot_n_tokens(subset2) <- c(50, 400))
  expect_error(tot_n_tokens(subset2) <- NA)
  expect_error(tot_n_tokens(subset2) <- "1")
  expect_error(tot_n_tokens(subset2) <- -50)
  expect_error(tot_n_tokens(flist) <- 3)
})

test_that("orig_ranks works", {
  expect_null(orig_ranks(flist))
  expect_equal(orig_ranks(subset_flist), c(11, 9, 6))
  expect_equal(
    names(orig_ranks(subset_flist, with_names = TRUE)),
    c("once", "lived", "corpus"))
  expect_null(names(orig_ranks(subset_flist)))
  
  subset2 <- subset_flist
  expect_error(orig_ranks(subset2) <- c(1, 2, 3))
  orig_ranks(subset2) <- NULL
  expect_null(orig_ranks(subset2))
})

test_that("ranks works", {
  no_name <- ranks(flist)
  with_name <- ranks(flist, with_names = TRUE)
  expect_length(no_name, 19)
  expect_equal(no_name[1], 1)
  expect_match(names(with_name[1]), "a")
  expect_null(names(no_name))
})

## Subsetting methods ====
test_that("subsetting freqlists by position works", {
  expect_s3_class(keep_pos(flist, 1:3), "freqlist")
  expect_s3_class(drop_pos(flist, 1:3), "freqlist")
  
  expect_length(keep_pos(flist, 1:5), 5)
  expect_length(keep_pos(flist, 3), 1)
  expect_length(drop_pos(flist, 1:5), length(flist)-5)
  expect_length(flist[1:5], 5)
  expect_length(flist[1:5, invert = TRUE], length(flist)-5)
  expect_match(type_names(flist[3]), "after")
  
  # negative indices
  expect_equal(drop_pos(flist, 3), keep_pos(flist, -3))
  expect_equal(keep_pos(flist, -3), keep_pos(flist, 3, invert = TRUE))
  
  # indices beyond length
  expect_length(keep_pos(flist, 20), 0)
  expect_length(keep_pos(flist, 15:25), 5)
})

test_that("subsetting freqlist by type works", {
  matcher <- c("after", "corpus", "and")
  full_match <- keep_types(flist, matcher)
  
  expect_s3_class(full_match, "freqlist")
  expect_s3_class(drop_types(flist, matcher), "freqlist")
  
  expect_length(full_match, 3)
  expect_length(drop_types(flist, matcher), length(flist)-3)
  expect_length(keep_types(flist, c(matcher, 'test')), 4)
  expect_length(keep_types(flist, 1), 1)
  
  expect_length(flist['after'], 1)
  expect_length(flist['test'], 1)
  expect_equal(as_numeric(flist['test']), 0)
  expect_equal(tot_n_tokens(flist['test']), 21)
  expect_equal(orig_ranks(flist['test']), NA_integer_)
  
  expect_equal(drop_types(flist, matcher), keep_types(flist, matcher, invert = TRUE))
})

test_that("subsetting freqlist by regex works", {
  # TODO add tests for the PERL argument
  t_regex <- "^t"
  dots_regex <- "^....?$"
  t_match <- keep_re(flist, t_regex)
  dots_match <- keep_re(flist, dots_regex)
  dots_unmatch <- drop_re(flist, dots_regex)
  dots_unmatch2 <- keep_re(flist, dots_regex, invert = TRUE)
  
  expect_s3_class(t_match, "freqlist")
  expect_s3_class(dots_unmatch, "freqlist")
  
  expect_length(t_match, 5)
  expect_length(dots_match, 8)
  expect_length(dots_unmatch, 11)
  expect_length(dots_unmatch2, 11)
  expect_equal(dots_unmatch, dots_unmatch2)
  expect_warning(
    double_pattern <- keep_re(flist, c(dots_regex, t_regex))
  )
  expect_equal(double_pattern, dots_match)
  expect_equal(keep_re(flist, re(t_regex)), t_match)
  
  expect_match(type_names(t_match[3]), "time")
  expect_match(type_names(dots_match[3]), "once")
  expect_match(type_names(dots_unmatch[2]), "it")
  
  expect_length(keep_re(flist, "test"), 0)
  expect_equal(tot_n_tokens(keep_re(flist, "test")), 21)
  expect_length(drop_re(flist, "test"), length(flist))
  
  expect_equal(flist[re(t_regex)], t_match)
  expect_length(flist[re("^.$")], 1)
})

test_that("subsetting freqlist with logic works", {
  alternated <- keep_bool(flist, c(TRUE, FALSE))
  expect_s3_class(alternated, "freqlist")
  expect_s3_class(drop_bool(flist, c(TRUE, FALSE)), "freqlist")
  
  expect_equal(tot_n_tokens(alternated), 21)
  expect_length(alternated, 10)
  by_nchar <- nchar(flist) < 5
  expect_length(keep_bool(flist, by_nchar), sum(by_nchar))
  expect_length(drop_bool(flist, by_nchar), length(flist)-sum(by_nchar))
  
  expect_length(flist[c(TRUE, FALSE)], 10)
  expect_length(flist[by_nchar], sum(by_nchar))
  expect_equal(keep_bool(flist, TRUE), drop_bool(flist, FALSE))
  expect_equal(drop_bool(flist, by_nchar), keep_bool(flist, by_nchar, invert = TRUE))
})

test_that("subsetting methods throw errors", {
  expect_error(keep_pos(flist, "1"))
  expect_error(keep_pos(flist, 1, invert = NULL))
  expect_error(keep_pos(flist, 1, invert = "TRUE"))
  expect_error(keep_pos(flist, c(1, -1)))
  
  expect_error(keep_types(flist, "after", invert = NULL))
  expect_error(keep_types(flist, "after", invert = "TRUE"))
  
  expect_error(keep_re(flist, 1))
  expect_error(keep_re(flist, NA))
  expect_error(keep_re(flist, "test", perl = "TRUE"))
  expect_error(keep_re(flist, "test", invert = "TRUE"))
  expect_warning(keep_re(flist, "test", perl = c(TRUE, FALSE)))
  expect_warning(keep_re(flist, "test", invert = c(TRUE, FALSE)))
  
  expect_error(keep_bool(flist, 1))
  expect_error(keep_bool(flist, "TRUE"))
  expect_error(keep_bool(flist, NULL))
  expect_error(keep_bool(flist, c(TRUE, NA)))
  expect_error(keep_bool(flist, c(TRUE, FALSE), invert = "TRUE"))
  expect_warning(keep_bool(flist, c(TRUE, FALSE), invert = c(TRUE, FALSE)))
  
  expect_error(flist[c(1, -1)])
  expect_error(flist[list('a', 'b')])
  
  expect_error(drop_pos(flist, invert = TRUE))
  expect_error(drop_types(flist, invert = TRUE))
  expect_error(drop_re(flist, invert = TRUE))
  expect_error(drop_bool(flist, invert = TRUE))
})

# Merging and other functions ====

test_that("type_freqs works", {
  expect_equal(type_freqs(flist)[2], 2)
  expect_equal(type_freq(flist)[2], 2)
  expect_equal(sum(type_freqs(flist)), 21)
  expect_length(type_freqs(flist), 19)
  expect_equal(sum(type_freqs(subset_flist)), 3)
  
  expect_match(names(type_freqs(flist, with_names = TRUE))[1], 'a')
  expect_null(names(type_freqs(flist)))
  
  expect_error(type_freqs(toy_corpus), "class 'freqlist'")
  expect_error(type_freqs(as_types(toy_corpus)), "class 'freqlist'")
  expect_error(type_freqs(tokenize(toy_corpus)), "class 'freqlist'")
})

test_that("merging works", {
  merge1 <- freqlist_merge(flist, subset_flist)
  expect_s3_class(merge1, "freqlist")
  expect_length(merge1, 19)
  expect_equal(tot_n_tokens(merge1), 42)
  expect_equal(n_tokens(merge1), 24)
  expect_equal(as_numeric(merge1[3]), 2)
  expect_match(type_names(merge1[3]), 'it')
  
  merge2 <- freqlist_merge_all(flist, subset_flist, subset_flist)
  expect_s3_class(merge2, "freqlist")
  expect_length(merge2, 19)
  expect_equal(tot_n_tokens(merge2), 63)
  expect_equal(n_tokens(merge2), 27)
  expect_equal(as_numeric(merge2[3]), 3)
  expect_match(type_names(merge2[3]), 'once')
  
  expect_error(freqlist_merge(flist, toy_corpus))
  expect_error(freqlist_merge(flist, subset_flist, subset_flist))
  expect_error(freqlist_merge(flist, subset_flist, toy_corpus))
})

test_that("freqlist difference works", {
  fdiff <- freqlist_diff(flist, subset_flist)
  expect_s3_class(fdiff, "freqlist")
  expect_length(fdiff, 19)
  expect_equal(n_tokens(fdiff), 18)
  expect_equal(tot_n_tokens(fdiff), 18)
  expect_null(orig_ranks(fdiff))
  
  expect_error(freqlist_diff(flist, toy_corpus))
})

# Arguments that come from tokenize/freqlist are tested with those classes
# QUESTION is it possible to test an interactive function like explore()?
# 
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentences. And it lived happily ever after."

tps <- types(toy_corpus, as_text = TRUE)
chr <- c('one', 'one', 'two', 'three', 'two')

# General methods ====

test_that("types are properly created", {
  expect_s3_class(tps, "types")
  expect_s3_class(tps, "character")
  expect_length(tps, 19)
  expect_length(tps[], 19)
})

test_that("types are printed properly", {
  expect_output(print(tps), "Type collection of length 19")
  expect_output(print(tps, n = 3), "Type collection of length 19")
  expect_output(print(tps), "type")
  expect_output(print(tps), "after")
  
  # Errors and warnings
  expect_error(print(tps, n = numeric()))
  expect_error(print(tps, n = NA))
  expect_error(print(tps, n = "1"))
  expect_warning(print(tps, n = c(5, 20)))
  
  expect_warning(print(tps, from = c(1, 3)))
  expect_error(print(tps, from = "1"))
  
  expect_warning(print(tps, sort = "alfa"))
  # TODO test extra argument?
})

test_that("dataframe conversion of types works", {
  expect_length(as.data.frame(tps), 1)
  expect_s3_class(as.data.frame(tps), "data.frame")
  expect_equal(nrow(as.data.frame(tps)), 19)
  
  expect_s3_class(as_tibble(tps), "tbl_df")
  expect_equal(nrow(as_tibble(tps)), 19)
})

test_that("types sorting works", {
  expect_length(sort(tps), 19)
  expect_s3_class(sort(tps), "types")
  expect_match(sort(tps)[2], "after")
  expect_match(sort(tps, decreasing = TRUE)[2], "upon")
})

test_that("types summary works", {
  sum_tps <- summary(tps)
  expect_s3_class(sum_tps, "summary.types")
  expect_length(sum_tps, 2)
  expect_equal(sum_tps$n_items, 19)
  expect_equal(sum_tps$n_unique_items, 19)
  expect_output(print(sum_tps), "Type collection of length 19")
  
  with_dup <- summary(as_types(chr, remove_duplicates = FALSE))
  expect_equal(with_dup$n_items, 5)
  expect_equal(with_dup$n_unique_items, 3)
  expect_output(print(with_dup), "Type collection of length 5")
  expect_output(print(with_dup), "duplicates present and counted double")
})

# Basic mclm methods ====

test_that("as_types() works", {
  expect_equal(
    as_types(freqlist(toy_corpus, as_text = TRUE)),
    sort(tps)
  )
  
  from_chr <- as_types(chr)
  expect_length(from_chr, length(unique(chr)))
  expect_match(from_chr[2], 'three')
  expect_match(as_types(chr, sort = FALSE)[2], 'three')
  expect_match(
    as_types(chr, remove_duplicates = FALSE, sort = FALSE)[2],
    'one'
  )
})

test_that("n_types() works", {
  expect_equal(n_types(tps), 19)
  expect_equal(n_types(as_types(chr)), 3)
  expect_warning(
    nt <- n_types(as_types(chr, remove_duplicates = FALSE)),
    "duplicates detected and counted double in 'types' object"
    )
  expect_equal(nt, 5)
})

## Subsetting methods ====
test_that("subsetting types by position works", {
  expect_s3_class(keep_pos(tps, 1:3), "types")
  expect_s3_class(drop_pos(tps, 1:3), "types")
  
  expect_length(keep_pos(tps, 1:5), 5)
  expect_length(keep_pos(tps, 3), 1)
  expect_length(drop_pos(tps, 1:5), length(tps)-5)
  expect_length(tps[1:5], 5)
  expect_length(tps[1:5, invert = TRUE], length(tps)-5)
  expect_match(tps[3], "after")
  
  # negative indices
  expect_equal(drop_pos(tps, 3), keep_pos(tps, -3))
  expect_equal(keep_pos(tps, -3), keep_pos(tps, 3, invert = TRUE))
  
  # indices beyond length
  expect_length(keep_pos(tps, 20), 0)
  expect_length(keep_pos(tps, 15:25), 5)
})

test_that("subsetting types by type works", {
  matcher <- c("after", "corpus", "and")
  full_match <- keep_types(tps, matcher)
  
  expect_s3_class(full_match, "types")
  expect_s3_class(drop_types(tps, matcher), "types")
  
  expect_length(full_match, 3)
  expect_match(full_match[2], 'and')
  expect_length(drop_types(tps, matcher), length(tps)-3)
  expect_length(keep_types(tps, c(matcher, 'test')), 3)
  expect_length(keep_types(tps, 1), 0)
  
  expect_match(tps['after'], 'after')
  expect_length(tps['after'], 1)
  expect_length(tps['test'], 0)
  
  expect_equal(drop_types(tps, matcher), keep_types(tps, matcher, invert = TRUE))
})

test_that("subsetting types by regex works", {
  # TODO add tests for the PERL argument
  t_regex <- "^t"
  dots_regex <- "^....?$"
  t_match <- keep_re(tps, t_regex)
  dots_match <- keep_re(tps, dots_regex)
  dots_unmatch <- drop_re(tps, dots_regex)
  dots_unmatch2 <- keep_re(tps, dots_regex, invert = TRUE)
  
  expect_s3_class(t_match, "types")
  expect_s3_class(dots_unmatch, "types")
  
  expect_length(t_match, 5)
  expect_length(dots_match, 8)
  expect_length(dots_unmatch, 11)
  expect_length(dots_unmatch2, 11)
  expect_equal(dots_unmatch, dots_unmatch2)
  expect_warning(
    double_pattern <- keep_re(tps, c(dots_regex, t_regex))
    )
  expect_equal(double_pattern, dots_match)
  expect_equal(keep_re(tps, re(t_regex)), t_match)
  
  expect_match(t_match[3], "time")
  expect_match(dots_match[3], "once")
  expect_match(dots_unmatch[2], "it")
  
  expect_length(keep_re(tps, "test"), 0)
  expect_length(drop_re(tps, "test"), length(tps))
  
  expect_equal(tps[re(t_regex)], t_match)
  expect_length(tps[re("^.$")], 1)
})

test_that("subsetting types with logic works", {
  alternated <- keep_bool(tps, c(TRUE, FALSE))
  expect_s3_class(alternated, "types")
  expect_s3_class(drop_bool(tps, c(TRUE, FALSE)), "types")
  
  expect_length(alternated, 10)
  by_nchar <- nchar(tps) < 5
  expect_length(keep_bool(tps, by_nchar), sum(by_nchar))
  expect_length(drop_bool(tps, by_nchar), length(tps)-sum(by_nchar))
  
  expect_length(tps[c(TRUE, FALSE)], 10)
  expect_length(tps[by_nchar], sum(by_nchar))
  expect_equal(keep_bool(tps, TRUE), drop_bool(tps, FALSE))
  expect_equal(drop_bool(tps, by_nchar), keep_bool(tps, by_nchar, invert = TRUE))
})

test_that("subsetting methods throw errors", {
  expect_error(keep_pos(tps, "1"))
  expect_error(keep_pos(tps, 1, invert = NULL))
  expect_error(keep_pos(tps, 1, invert = "TRUE"))
  expect_error(keep_pos(tps, c(1, -1)))
  
  expect_error(keep_types(tps, "after", invert = NULL))
  expect_error(keep_types(tps, "after", invert = "TRUE"))
  
  expect_error(keep_re(tps, 1))
  expect_error(keep_re(tps, NA))
  expect_error(keep_re(tps, "test", perl = "TRUE"))
  expect_error(keep_re(tps, "test", invert = "TRUE"))
  expect_warning(keep_re(tps, "test", perl = c(TRUE, FALSE)))
  expect_warning(keep_re(tps, "test", invert = c(TRUE, FALSE)))
  
  expect_error(keep_bool(tps, 1))
  expect_error(keep_bool(tps, "TRUE"))
  expect_error(keep_bool(tps, NULL))
  expect_error(keep_bool(tps, c(TRUE, NA)))
  expect_error(keep_bool(tps, c(TRUE, FALSE), invert = "TRUE"))
  expect_warning(keep_bool(tps, c(TRUE, FALSE), invert = c(TRUE, FALSE)))
  
  expect_error(tps[c(1, -1)])
  expect_error(tps[list('a', 'b')])
  
  expect_error(drop_pos(tps, invert = TRUE))
  expect_error(drop_types(tps, invert = TRUE))
  expect_error(drop_re(tps, invert = TRUE))
  expect_error(drop_bool(tps, invert = TRUE))
})

# Merging ====

test_that("merging works", {
  merge1 <- types_merge(tps, as_types(chr))
  expect_s3_class(merge1, "types")
  expect_length(merge1, 21)
  expect_match(merge1[20], "one")
  expect_match(types_merge(tps, as_types(chr), sort = TRUE)[20], "upon")
  
  merge2 <- types_merge_all(tps, as_types(chr), as_types(c("another", "word")))
  expect_s3_class(merge2, "types")
  expect_length(merge2, 23)
  expect_match(merge2[23], "word")
  
  expect_error(types_merge(tps, chr))
  expect_error(types_merge(tps, as_types(chr), as_types(c("another", "word"))))
  expect_error(types_merge_all(tps, chr))
  
})
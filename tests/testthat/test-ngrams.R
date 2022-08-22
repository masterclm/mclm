test_that("build_open_pos() works", {
  bop_4_3 <- build_open_pos(4, 3)
  expect_type(bop_4_3, "list")
  expect_length(bop_4_3, 0)
  
  bop_4_2 <- build_open_pos(4, 2)
  expect_type(bop_4_2, "list")
  expect_length(bop_4_2, 1)
  
  bop_5_2 <- build_open_pos(5, 2)
  expect_type(bop_5_2, "list")
  expect_length(bop_5_2, 3)
  expect_equal(min(vapply(bop_5_2, min, FUN.VALUE = double(1))), 2)
  expect_equal(max(vapply(bop_5_2, max, FUN.VALUE = double(1))), 4)
  
  bop_10_6 <- build_open_pos(10, 6)
  expect_type(bop_10_6, "list")
  expect_length(bop_10_6, 28)
  expect_equal(min(vapply(bop_10_6, min, FUN.VALUE = double(1))), 2)
  expect_equal(max(vapply(bop_10_6, max, FUN.VALUE = double(1))), 9)
  expect_equal(max(vapply(bop_10_6, min, FUN.VALUE = double(1))), 4)
  expect_equal(min(vapply(bop_10_6, max, FUN.VALUE = double(1))), 7)
  
})

toy_corpus <- "
  Once upon a time there was a tiny toy corpus.
  It consisted of three sentences.
  And it lived happily ever after.
  "
tks <- tokenize(toy_corpus)

test_that("basic ngrams are built", {
  n3 <- build_ngrams(tks)
  expect_length(n3, 19)
  expect_type(n3, "character")
  expect_match(n3[[1]], "once_upon_a")
  expect_match(n3[[2]], "upon_a_time")
  
  expect_match(build_ngrams(tks, sep = ":")[[1]], "once:upon:a")
})

test_that("basic skipgram works", {
  s3 <- build_ngrams(tks, max_skip = 1)
  expect_length(s3, 55)
  expect_type(s3, "character")
  expect_match(s3[[1]], "once_upon_a")
  expect_match(s3[[2]], "once")
  expect_match(s3[[3]], "once")
  
  expect_match(build_ngrams(tks, max_skip = 1, sep = ":")[[2]], "once:upon:time")
})

test_that("open ngrams work", {
  o52 <- build_ngrams(tks, ngram_size = 5, n_open = 2)
  expect_type(o52, "character")
  expect_length(o52, 51)
  expect_match(o52[[1]], "once_\\[]_\\[]_time_there")
  expect_match(o52[[2]], "upon_\\[]_\\[]_there_was")
  
  other_sep <- build_ngrams(tks, ngram_size = 5, n_open = 2, sep = ":")
  expect_type(other_sep, "character")
  expect_length(other_sep, 51)
  expect_match(other_sep[[1]], "once:\\[]:\\[]:time:there")
  expect_match(other_sep[[2]], "upon:\\[]:\\[]:there:was")
  
  other_open <- build_ngrams(tks, ngram_size = 5, n_open = 2,
                             sep = ":", open = "{}")
  expect_type(other_open, "character")
  expect_length(other_open, 51)
  expect_match(other_open[[1]], "once:\\{}:\\{}:time:there")
  expect_match(other_open[[2]], "upon:\\{}:\\{}:there:was")
})
# Arguments that come from tokenize/freqlist are tested with those classes
# QUESTION is it possible to test an interactive function like explore()?
# 
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentences. And it lived happily ever after."

tps <- types(toy_corpus, as_text = TRUE)
chr <- c('one', 'one', 'two', 'three', 'two')

test_that("types are properly created", {
  expect_s3_class(tps, "types")
  expect_s3_class(tps, "character")
  expect_length(tps, 19)
})

test_that("types are printed properly", {
  expect_output(print(tps), "Type collection of length 19")
  expect_output(print(tps), "type")
  expect_output(print(tps), "after")
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

test_that("subsetting types by position works", {
  expect_length(keep_pos(tps, 1:5), 5)
  expect_length(keep_pos(tps, 3), 1)
  expect_length(drop_pos(tps, 1:5), length(tps)-5)
  
  # negative indices
  expect_equal(drop_pos(tps, 3), keep_pos(tps, -3))
  expect_equal(keep_pos(tps, -3), keep_pos(tps, 3, invert = TRUE))
  
  # indices beyond length
  expect_length(keep_pos(tps, 20), 0)
  expect_length(keep_pos(tps, 15:25), 5)
})

test_that("subsetting methods throw errors", {
  expect_error(keep_pos(tps, "1"))
  expect_error(keep_pos(tps, invert = NULL))
  expect_error(keep_pos(tps, invert = "TRUE"))
  expect_error(keep_pos(tps, c(1, -1)))
})
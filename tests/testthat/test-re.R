# TODO test groups other than 1
# TODO test useBytes = TRUE
x <- tokenize("This is a sentence with a couple of words in it.")
pattern <- "[oe](.)(.)"

test_that("re_retrieve_first works", {
  res <- re_retrieve_first(x, pattern)
  expect_length(res, length(x))
  expect_equal(res[[1]], NA_character_)
  expect_match(res[[4]], "ent")
  
  res_drop_na <- re_retrieve_first(x, pattern, drop_NA = TRUE)
  expect_length(res_drop_na, length(res[!is.na(res)]))
  
  purrr::walk2(res_drop_na, res[!is.na(res)], expect_equal)
})

test_that("re_retrieve_first with group capture works", {
  res <- re_retrieve_first(x, pattern, requested_group = 1)
  expect_length(res, length(x))
  expect_equal(res[[1]], NA_character_)
  expect_equal(res[[4]], "n")
  
  res_drop_na <- re_retrieve_first(x, pattern, drop_NA = TRUE,
                                   requested_group = 1)
  expect_length(res_drop_na, length(res[!is.na(res)]))
  
  purrr::walk2(res_drop_na, res[!is.na(res)], expect_equal)
})

test_that("re_retrieve_last works", {
  res <- re_retrieve_last(x, pattern)
  expect_length(res, length(x))
  expect_equal(res[[1]], NA_character_)
  expect_match(res[[4]], "enc")
  
  res_drop_na <- re_retrieve_last(x, pattern, drop_NA = TRUE)
  expect_length(res_drop_na, length(res[!is.na(res)]))
  
  purrr::walk2(res_drop_na, res[!is.na(res)], expect_equal)
})

test_that("re_retrieve_last with group capture works", {
  res <- re_retrieve_last(x, pattern, requested_group = 1)
  expect_length(res, length(x))
  expect_equal(res[[1]], NA_character_)
  expect_equal(res[[4]], "n")
  
  res_drop_na <- re_retrieve_last(x, pattern, drop_NA = TRUE,
                                   requested_group = 1)
  expect_length(res_drop_na, length(res[!is.na(res)]))
  
  purrr::walk2(res_drop_na, res[!is.na(res)], expect_equal)
})

test_that("re_retrieve_last with different group capture works", {
  res <- re_retrieve_last(x, pattern, requested_group = 2)
  expect_length(res, length(x))
  expect_equal(res[[1]], NA_character_)
  expect_equal(res[[4]], "c")
  
  res_drop_na <- re_retrieve_last(x, pattern, drop_NA = TRUE,
                                  requested_group = 2)
  expect_length(res_drop_na, length(res[!is.na(res)]))
  
  purrr::walk2(res_drop_na, res[!is.na(res)], expect_equal)
})

test_that("re_retrieve_all works", {
  res <- re_retrieve_all(x, pattern)
  expect_length(res, 4)
  expect_match(res[[1]], "ent")
  
  res_list <- re_retrieve_all(x, pattern, unlist = FALSE)
  expect_length(res_list, length(x))
  expect_length(res_list[[1]], 0)
  expect_length(res_list[[4]], 2)
  expect_length(res_list[[7]], 1)
  
  purrr::walk2(unlist(res_list), res, expect_equal)
})

test_that("re_retrieve_all with group capture works", {
  res <- re_retrieve_all(x, pattern, requested_group = 1)
  expect_length(res, 4)
  expect_match(res[[1]], "n")
  
  res_list <- re_retrieve_all(x, pattern, unlist = FALSE,
                              requested_group = 1)
  expect_length(res_list, length(x))
  expect_length(res_list[[1]], 0)
  expect_length(res_list[[4]], 2)
  expect_length(res_list[[7]], 1)
  
  purrr::walk2(unlist(res_list), res, expect_equal)
})



test_that("re_replace_ functions work", {
  res_first <- re_replace_first(x, "([oe].)", "{\\1}")
  expect_length(res_first, length(x))
  expect_s3_class(res_first, "tokens")
  expect_equal(res_first[[1]], x[[1]])
  expect_equal(res_first[[4]], "s{en}tence")
  
  res_all <- re_replace_all(x, "([oe].)", "{\\1}")
  res_all <- re_replace_all(x, "([oe].)", "{\\1}")
  expect_length(res_all, length(x))
  expect_s3_class(res_all, "tokens")
  expect_equal(res_all[[1]], x[[1]])
  expect_equal(res_all[[4]], "s{en}t{en}ce")
})
test_that("get_fnames works properly", {
  test_files <- get_fnames(system.file("extdata", "cleveland", package = "mclm"))
  expect_s3_class(test_files, "fnames")
  expect_length(test_files, 4)

  expect_match(test_files[[1]], "cleveland_speeches_000")
  
  # pattern
  file000 <- get_fnames(system.file("extdata", package = "mclm"),
                        re_pattern = "000")
  expect_length(file000, 2)
  expect_match(file000[2], "roosevelt")
  
  # recursive
  expect_length(
    get_fnames(system.file("extdata", package = "mclm"),
               recursive = FALSE),
    2
  )
  # TODO create directory for other tests and use it to test recursive
})

test_that("as_fnames converts properly", {
  expect_s3_class(as_fnames(chr), "fnames")
  expect_length(as_fnames(chr), 3)
  expect_length(as_fnames(chr, remove_duplicates = FALSE), 5)
  
  
  expect_length(flist_fnames, 19)
  expect_length(as_fnames(NULL), 0)
  expect_length(as_fnames(2), 1)
})

# Generic methods ====

test_that("fnames are printed properly", {
  expect_output(print(flist_fnames), "Filename collection of length 19")
  expect_output(print(flist_fnames, n = 0), "Filename collection of length 19")
  expect_output(print(flist_fnames, n = 3), "Filename collection of length 19")
  expect_output(print(flist_fnames), "filename")
  expect_output(print(flist_fnames), "after")
  
  # Errors and warnings
  expect_error(print(flist_fnames, n = numeric()))
  expect_error(print(flist_fnames, n = NA))
  expect_error(print(flist_fnames, n = "1"))
  expect_warning(print(flist_fnames, n = c(5, 20)))
  
  expect_warning(print(flist_fnames, from = c(1, 3)))
  expect_error(print(flist_fnames, from = "1"))
  
  expect_warning(print(flist_fnames, sort = "alfa"))
  # TODO test extra argument?
})

test_that("dataframe conversion of fnames works", {
  expect_length(as.data.frame(flist_fnames), 1)
  expect_s3_class(as.data.frame(flist_fnames), "data.frame")
  expect_equal(nrow(as.data.frame(flist_fnames)), 19)
  
  expect_s3_class(as_tibble(flist_fnames), "tbl_df")
  expect_equal(nrow(as_tibble(flist_fnames)), 19)
})

test_that("fnames sorting works", {
  expect_length(sort(flist_fnames), 19)
  expect_s3_class(sort(flist_fnames), "fnames")
  expect_match(sort(flist_fnames)[2], "after")
  expect_match(sort(flist_fnames, decreasing = TRUE)[2], "upon")
  # TODO test na.last
})

test_that("fnames summary works", {
  sum_flist_fnames <- summary(flist_fnames)
  expect_s3_class(sum_flist_fnames, "summary.fnames")
  expect_length(sum_flist_fnames, 2)
  expect_equal(sum_flist_fnames$n_items, 19)
  expect_equal(sum_flist_fnames$n_unique_items, 19)
  expect_output(print(sum_flist_fnames), "Filename collection of length 19")
  
  with_dup <- summary(as_fnames(chr, remove_duplicates = FALSE))
  expect_equal(with_dup$n_items, 5)
  expect_equal(with_dup$n_unique_items, 3)
  expect_output(print(with_dup), "Filename collection of length 5")
  expect_output(print(with_dup), "duplicates present and counted double")
})

## Subsetting methods ====
test_that("subsetting fnames by position works", {
  expect_s3_class(keep_pos(flist_fnames, 1:3), "fnames")
  expect_s3_class(drop_pos(flist_fnames, 1:3), "fnames")
  
  expect_length(keep_pos(flist_fnames, 1:5), 5)
  expect_length(keep_pos(flist_fnames, 3), 1)
  expect_match(keep_pos(flist_fnames, 3), "and")
  expect_length(drop_pos(flist_fnames, 1:5), length(flist_fnames)-5)
  expect_length(flist_fnames[1:5], 5)
  expect_length(flist_fnames[1:5, invert = TRUE], length(flist_fnames)-5)
  expect_match(flist_fnames[3], "and")
  
  # negative indices
  expect_equal(drop_pos(flist_fnames, 3), keep_pos(flist_fnames, -3))
  expect_equal(keep_pos(flist_fnames, -3), keep_pos(flist_fnames, 3, invert = TRUE))
  
  # indices beyond length
  expect_length(keep_pos(flist_fnames, 20), 0)
  expect_length(keep_pos(flist_fnames, 15:25), 5)
})

test_that("subsetting fnames by type works", {
  matcher <- c("after", "consisted", "corpus")
  full_match <- keep_types(flist_fnames, matcher)
  
  expect_s3_class(full_match, "fnames")
  expect_s3_class(drop_types(flist_fnames, matcher), "fnames")
  
  expect_length(full_match, 3)
  expect_length(drop_types(flist_fnames, matcher), length(flist_fnames)-3)
  expect_length(keep_types(flist_fnames, c(matcher, 'test')), 3)
  expect_length(keep_types(flist_fnames, 1), 0)
  
  expect_length(flist_fnames['lived'], 1)
  expect_length(flist_fnames['test'], 0)
  
  expect_equal(drop_types(flist_fnames, matcher),
               keep_types(flist_fnames, matcher, invert = TRUE))
})

test_that("subsetting fnames by regex works", {
  # TODO add tests for the PERL argument
  t_regex <- "^t"
  dots_regex <- "^....?$"
  t_match <- keep_re(flist_fnames, t_regex)
  dots_match <- keep_re(flist_fnames, dots_regex)
  dots_unmatch <- drop_re(flist_fnames, dots_regex)
  dots_unmatch2 <- keep_re(flist_fnames, dots_regex, invert = TRUE)
  
  expect_s3_class(t_match, "fnames")
  expect_s3_class(dots_unmatch, "fnames")
  
  expect_length(t_match, 5)
  expect_length(dots_match, 8)
  expect_length(dots_unmatch, 11)
  expect_length(dots_unmatch2, 11)
  expect_equal(dots_unmatch, dots_unmatch2)
  expect_warning(
    double_pattern <- keep_re(flist_fnames, c(dots_regex, t_regex))
  )
  expect_equal(double_pattern, dots_match)
  expect_equal(keep_re(flist_fnames, re(t_regex)), t_match)
  
  expect_match(t_match[1], "there")
  expect_match(dots_match[3], "once")
  expect_match(dots_unmatch[2], "after")
  
  expect_length(keep_re(flist_fnames, "test"), 0)
  expect_length(drop_re(flist_fnames, "test"), length(flist_fnames))
  
  expect_equal(flist_fnames[re(t_regex)], t_match)
  expect_length(flist_fnames[re("^.$")], 1)
})

test_that("subsetting fnames with logic works", {
  alternated <- keep_bool(flist_fnames, c(TRUE, FALSE))
  expect_s3_class(alternated, "fnames")
  expect_s3_class(drop_bool(flist_fnames, c(TRUE, FALSE)), "fnames")
  
  expect_length(alternated, 10)
  by_nchar <- nchar(flist_fnames) < 5
  expect_length(keep_bool(flist_fnames, by_nchar), sum(by_nchar))
  expect_length(drop_bool(flist_fnames, by_nchar), length(flist_fnames)-sum(by_nchar))
  
  expect_length(flist_fnames[c(TRUE, FALSE)], 10)
  expect_length(flist_fnames[by_nchar], sum(by_nchar))
  expect_equal(keep_bool(flist_fnames, TRUE), drop_bool(flist_fnames, FALSE))
  expect_equal(drop_bool(flist_fnames, by_nchar), keep_bool(flist_fnames, by_nchar, invert = TRUE))
})

test_that("subsetting methods throw errors", {
  expect_error(keep_pos(flist_fnames, "1"))
  expect_error(keep_pos(flist_fnames, 1, invert = NULL))
  expect_error(keep_pos(flist_fnames, 1, invert = "TRUE"))
  expect_error(keep_pos(flist_fnames, c(1, -1)))
  
  expect_error(keep_types(flist_fnames, "after", invert = NULL))
  expect_error(keep_types(flist_fnames, "after", invert = "TRUE"))
  
  expect_error(keep_re(flist_fnames, 1))
  expect_error(keep_re(flist_fnames, NA))
  expect_error(keep_re(flist_fnames, "test", perl = "TRUE"))
  expect_error(keep_re(flist_fnames, "test", invert = "TRUE"))
  expect_warning(keep_re(flist_fnames, "test", perl = c(TRUE, FALSE)))
  expect_warning(keep_re(flist_fnames, "test", invert = c(TRUE, FALSE)))
  
  expect_error(keep_bool(flist_fnames, 1))
  expect_error(keep_bool(flist_fnames, "TRUE"))
  expect_error(keep_bool(flist_fnames, NULL))
  expect_error(keep_bool(flist_fnames, c(TRUE, NA)))
  expect_error(keep_bool(flist_fnames, c(TRUE, FALSE), invert = "TRUE"))
  expect_warning(keep_bool(flist_fnames, c(TRUE, FALSE), invert = c(TRUE, FALSE)))
  
  expect_error(flist_fnames[c(1, -1)])
  expect_error(flist_fnames[list('a', 'b')])
  
  expect_error(drop_pos(flist_fnames, invert = TRUE))
  expect_error(drop_types(flist_fnames, invert = TRUE))
  expect_error(drop_re(flist_fnames, invert = TRUE))
  expect_error(drop_bool(flist_fnames, invert = TRUE))
})

# Other functions ====
test_that("drop_path works well", {
  dropped <- drop_path(cwd_fnames)
  expect_s3_class(dropped, "fnames")
  expect_match(dropped[[1]], "^file1\\.txt$")
  expect_length(dropped, length(cwd_fnames))
})

test_that("drop_extension works well", {
  dropped <- drop_extension(cwd_fnames)
  expect_s3_class(dropped, "fnames")
  expect_match(dropped[[1]], "^folder/file1$")
  expect_length(dropped, length(cwd_fnames))
  drop_extension(cwd_fnames)
  short_names(cwd_fnames) # same as drop_path(drop_extension(cwd_fnames))
})

test_that("short_names works well", {
  dropped <- short_names(cwd_fnames)
  expect_s3_class(dropped, "fnames")
  expect_match(dropped[[1]], "^file1$")
  expect_length(dropped, length(cwd_fnames))
})

## Setters and getters ====
test_that("n_fnames works properly", {
  expect_equal(n_fnames(cwd_fnames), 3)
  expect_equal(n_fnames(flist_fnames), 19)
  expect_error(n_fnames(flist))
  expect_error(n_fnames(tps))
  
  with_dup <- as_fnames(chr, remove_duplicates = FALSE)
  expect_warning(dupped <- n_fnames(with_dup), "duplicates detected")
  expect_equal(dupped, 5)
})

test_that("merging fnames works", {
  chr_fnames <- as_fnames(chr)
  merge1 <- fnames_merge(flist_fnames, chr_fnames)
  expect_s3_class(merge1, "fnames")
  expect_length(merge1, 21)
  expect_match(merge1[20], "one")
  expect_match(fnames_merge(flist_fnames, chr_fnames, sort = TRUE)[20], "upon")
  
  merge2 <- fnames_merge_all(flist_fnames, chr_fnames, as_fnames(c("another", "word")))
  expect_s3_class(merge2, "fnames")
  expect_length(merge2, 23)
  expect_match(merge2[23], "word")
  
  expect_error(fnames_merge(flist_fnames, chr))
  expect_error(fnames_merge(flist_fnames, chr_fnames, as_fnames(c("another", "word"))))
  expect_error(fnames_merge_all(flist_fnames, chr))
})

test_that("keep_fnames and drop_fnames work", {
  all_fnames <- as_fnames(c("file1", "file2", "file3",
                            "file4", "file5", "file6"))
  
  unwanted_fnames <- as_fnames(c("file1", "file4"))
  
  drop1 <- keep_fnames(all_fnames, unwanted_fnames, invert = TRUE)
  drop2 <- drop_fnames(all_fnames, unwanted_fnames)
  expect_length(drop1, 4)
  expect_match(drop1[[1]], "file2")
  expect_match(drop1[[4]], "file6")
  expect_equal(drop1, drop2)
  expect_equal(drop1, drop_fnames(all_fnames, c("file1", "file4")))
  
  
  wanted_fnames <- as_fnames(c("file3", "file5"))
  keep <- keep_fnames(all_fnames, wanted_fnames)
  expect_length(keep, 2)
  expect_match(keep[[1]], "file3")
  
  expect_error(keep_fnames(chr, unwanted_fnames))
  expect_error(keep_fnames(all_fnames, unwanted_fnames, invert = NULL))
  expect_error(keep_fnames(all_fnames, unwanted_fnames, invert = "TRUE"))
})

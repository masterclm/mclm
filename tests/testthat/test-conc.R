# QUESTION test for explore()?
# TODO test perl = FALSE?

test_that("A concordance is built correctly", {
  expect_s3_class(conc_data, "conc")
  expect_s3_class(conc_data, "data.frame")
  
  expect_length(conc_data, 6)
  expect_length(conc_data$match, 4)
  expect_match(conc_data$source[[1]], "-")
  expect_match(conc_data$match[[1]], "A")
  
  short_win <- conc(
    "A very small corpus", "\\w+",
    c_left = 3, c_right = 3, as_text = TRUE)
  expect_match(short_win$left[[3]], "^ry $")
  expect_match(short_win$right[[3]], "^ co$")
  
  # drop line
  expect_length(
    conc(
      lines_text,
      "\\bt[a-z]+\\b",
      as_text = TRUE)$match,
    7
  )
  expect_length(
    conc(
      lines_text,
      "\\bt[a-z]+\\b",
      re_drop_line = "</?[a-z]+>",
      as_text = TRUE)$match,
    5
  )
  
  expect_match(
    conc(lines_text, "This",
         re_cut_area = "text", as_text = TRUE)$right[[1]],
    "is a with more"
  )
})

test_that("conc objects print well", {
  expect_output(print(conc_data), "Concordance-based data frame")
  expect_output(print(conc_data), "number of observations: 4")
  expect_output(print(conc_data), "idx")
  expect_output(print(conc_data), "This data frame has 6 columns")
  expect_output(
    print(mutate(conc_data, MATCH = toupper(match))),
    "This data frame has 7 columns"
  )
  
  expect_output(print(conc_data, n = 2), "...")
})

test_that("print_kwic works well", {
  expect_error(print_kwic("some character"))
  expect_error(print_kwic(conc(lines_text, "test", as_text = TRUE)), "appropriate values")
  expect_error(print_kwic(conc_data, n = "1"))
  expect_error(print_kwic(conc_data, n = numeric()))
  expect_error(print_kwic(conc_data, from = numeric()))
  
  expect_output(print_kwic(conc_data), "idx")
  expect_output(print_kwic(conc_data), "A very small")
  expect_output(print_kwic(conc_data, max_c_left = 3), "...l")
})

test_that("dataframe conversion of concordances works", {
  expect_length(as.data.frame(conc_data), 6)
  expect_s3_class(as.data.frame(conc_data), "data.frame")
  expect_equal(nrow(as.data.frame(conc_data)), 4)
  
  expect_s3_class(as_tibble(conc_data), "tbl_df")
  expect_equal(nrow(as_tibble(conc_data)), 4)
})

test_that("concordances are merged properly", {
  merged <- merge_conc(conc_data, conc(lines_text, "This", as_text = TRUE))
  expect_s3_class(merged, "conc")
  expect_length(merged, 6)
  expect_equal(nrow(merged), 7)
})

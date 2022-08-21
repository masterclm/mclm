# TODO test small_pos or whatever strategy is used eventually
# ngram part is tested by ngrams code
test_that("default slma is performed correctly", {
  expect_output(slma_ex <- slma(a_corp, b_corp),
                "building global frequency list for x")
  expect_silent(slma(a_corp, b_corp, verbose = FALSE))
  
  expect_s3_class(slma_ex, "slma")
  expect_length(slma_ex, 6)
  
  scores <- slma_ex$scores
  expect_s3_class(scores, "data.frame")
  expect_length(scores, 8)
  expect_equal(nrow(scores), 4073)
  expect_match(rownames(scores)[[1]], "the")
  expect_null(slma_ex$intermediate)
  
  # Test some values
  ex_1 <- slma_ex$scores["government",]
  expect_equal(ex_1$S_abs, 13)
  expect_equal(ex_1$S_nrm, 0.65)
  expect_equal(ex_1$S_att, 13)
  expect_equal(ex_1$S_rep, 0)
  expect_equal(ex_1$S_lor, 1.112, tolerance = 0.001)
  expect_equal(ex_1$lor_min, 0.785, tolerance = 0.001)
  expect_equal(ex_1$lor_max, 3.172, tolerance = 0.001)
  expect_equal(ex_1$lor_sd, 0.798, tolerance = 0.001)
  
  expect_equal(sum(scores$S_att > scores$S_rep), 780)
  expect_equal(sum(scores$S_att < scores$S_rep), 312)
})

test_that("slma with different cutoff works properly", {
  slma_ex <- slma(a_corp, b_corp, sig_cutoff = qchisq(.97, df = 1))
  scores <- slma_ex$scores
  
  expect_equal(nrow(scores), 4073)
  ex_1 <- slma_ex$scores["government",]
  expect_equal(ex_1$S_abs, 11)
  expect_equal(ex_1$S_nrm, 0.55)
  expect_equal(ex_1$S_att, 11)
  expect_equal(ex_1$S_rep, 0)
  expect_equal(ex_1$S_lor, 1.016, tolerance = 0.001)
  expect_equal(ex_1$lor_min, 0.785, tolerance = 0.001)
  expect_equal(ex_1$lor_max, 3.172, tolerance = 0.001)
  expect_equal(ex_1$lor_sd, 0.790, tolerance = 0.001)
  
  expect_equal(sum(scores$S_att > scores$S_rep), 750)
  expect_equal(sum(scores$S_att < scores$S_rep), 248)
})

test_that("slma with keeplist and stoplist works properly", {
  slma_ex <- slma(
    a_corp, b_corp,
    keeplist = c("justice", "government", "trust"),
    verbose = FALSE)
  expect_equal(nrow(slma_ex$scores), 3)
  expect_match(rownames(slma_ex$scores)[[1]], "justice")
  expect_equal(slma_ex$scores$S_abs[[1]], 6)
  
  expect_false(
    "government" %in% rownames(slma(
      a_corp, b_corp,
      stoplist = "government", verbose = FALSE
      )$scores)
  )
  expect_equal(
    nrow(slma(
      a_corp, b_corp,
      stoplist = "government", keeplist = "government")$scores
      ),
    1
  )
})

test_that("rank settings work for slma", {
  expect_equal(
    nrow(slma(a_corp, b_corp, verbose = FALSE, min_rank = 100)$scores),
    3974
  )
  expect_equal(
    nrow(slma(a_corp, b_corp, verbose = FALSE, max_rank = 100)$scores),
    100
  )
  
  expect_equal(
    nrow(slma(
      a_corp, b_corp, verbose = FALSE,
      min_rank = 50, max_rank = 100)$scores),
    51
  )
})

test_that("slma details are created properly", {
  expect_error(
    details(slma(a_corp, b_corp, verbose = FALSE), "government"),
    "keep_intermediate")
  
  with_intermediate <- slma(a_corp, b_corp, verbose = FALSE,
                            keep_intermediate = TRUE,
                            keeplist = c("justice", "government", "trust"))
  expect_length(with_intermediate, 6)
  expect_length(with_intermediate$intermediate, 2)
  expect_length(with_intermediate$intermediate$marker_freqs, 9)
  expect_equal(with_intermediate$intermediate$tot_n_tokens[[1]], 1692)
  
  gov <- details(with_intermediate, "government")
  expect_s3_class(gov, "details.slma")
  expect_length(gov, 5)
  expect_equal(gov$sig_cutoff, qchisq(.95, df = 1))
  
  expect_s3_class(gov$summary, "data.frame")
  expect_length(gov$summary, 8)
  expect_equal(nrow(gov$summary), 1)
  
  expect_length(gov$scores, 10)
  expect_equal(nrow(gov$scores), 20)
  expect_match(
    rownames(gov$scores)[[1]],
    "cleveland_speeches_000--roosevelt_speeches_000"
    )
  expect_equal(gov$scores$a[[1]], 16)
  
  expect_output(print(gov), "SLMA details")
  expect_output(print(gov), 'item: "government"')
  expect_output(print(gov), "cutoff for G: 3.841")
  
  long_names <- rownames(details(with_intermediate, "government", shorten_names = FALSE)$scores)
  expect_match(long_names[[1]], paste(a_corp[[1]], b_corp[[1]], sep = "--"))
})

test_that("slma is properly turned to data frame", {
  slma_ex <- slma(a_corp, b_corp,
                  keeplist = c("justice", "government", "trust"),
                  verbose = FALSE)
  as_df <- as.data.frame(slma_ex)
  expect_s3_class(as_df, "data.frame")
  expect_length(as_df, 9)
  expect_equal(nrow(as_df), 3)
  expect_match(colnames(as_df)[[1]], "type")
  expect_equal(as_df$S_abs[[1]], 13)
  
  expect_s3_class(as_tibble(slma_ex), "tbl_df")
  expect_length(as_tibble(slma_ex), 9)
})

# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

meanC <- function(x) {
    .Call(`_mclm_meanC`, x)
}

test1 <- function(x) {
    .Call(`_mclm_test1`, x)
}

skipgramsC_orig <- function(x_len, ngram_size, max_skip) {
    .Call(`_mclm_skipgramsC_orig`, x_len, ngram_size, max_skip)
}

skipgramsC <- function(x, ngram_size, max_skip, sep) {
    .Call(`_mclm_skipgramsC`, x, ngram_size, max_skip, sep)
}


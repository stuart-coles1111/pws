ht_max_run_cdf <- function(n, x) {
ifelse(x == 0, 0, pws:::h_max_run_cdf(n - 1, x - 1))
}

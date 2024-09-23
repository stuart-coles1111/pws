var_max_run_length <- function(n) {
    v <- 0
    for (i in 1:n) {
        v <- v + i ^ 2 * (pws:::ht_max_run_cdf(n, i) - pws:::ht_max_run_cdf(n, i - 1))
    }
    v <- v - pws:::mean_max_run_length(n) ^ 2
    v
}

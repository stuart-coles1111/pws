mean_max_run_length <- function(n) {
    m <- 1
    for (i in 1:n)
        m <-  m + 1 - pws:::ht_max_run_cdf(n, i)
    m
}

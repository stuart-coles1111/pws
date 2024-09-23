h_max_run_cdf <- function(n, x) {
    if (x >= n)
        cdf <- 1
    else
    {
        A <- c()
        A[1:(x + 1)] <- 2 ^ (0:x)
        for (i in (x + 2):(n + 1))
            A[i] <- sum(A[(i - (x + 1)):(i - 1)])
        cdf <- A[n + 1] / (2 ^ n)
    }
    cdf
}

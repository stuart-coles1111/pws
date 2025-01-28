ski_jump <- function(a1, njump = 3, weight = c(4, 2, 3), mu = 200, sd = 10){
    m <- pws:::ski_strengths(a1, weight)[[2]][1]
    d <- mu + rnorm(njump, m, sd)
    d %>% round(2)
}

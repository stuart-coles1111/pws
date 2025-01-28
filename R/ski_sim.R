ski_sim <- function(weight = c(4 , 2 , 3), mu = 200, sd=10){
    a1 <- runif(3, 0, 10)
    m <- pws:::ski_strengths(a1, weight)[[2]][1]
    d <- mu + rnorm(1, m, sd)
    c(a1, d) %>% round(2)
}

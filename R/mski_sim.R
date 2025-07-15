mski_sim <- function(n, weight = c(40, 20, 30), mu = 200, sd = 10){
    df <- lapply(1:n,function(x, weight, mu, sd) pws:::ski_sim(weight, mu, sd), weight = weight, mu = mu, sd = sd)
    df <- do.call(rbind, df) %>% as.data.frame
    colnames(df) <- c('Technique', 'Materials', 'Fitness', 'Jump_Length')
    df
}

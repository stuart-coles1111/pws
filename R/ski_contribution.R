ski_contribution <- function(x, weight = 10){
    ifelse(x < 0,-999, ifelse(x > 10, -999 , weight * x))
}

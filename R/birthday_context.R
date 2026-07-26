birthday_context <- function(matches = 4,
                             group_sizes = c(20,100),
                             method = c("poisson","dp")) {

    method <- match.arg(method)

    probs <- sapply(group_sizes, function(n){

        if(method == "dp"){
            birthday_dp(n, matches)
        } else {
            birthday_poisson(n, matches)
        }

    })

    data.frame(
        group = group_sizes,
        probability = probs,
        k = 1 / probs
    )
}

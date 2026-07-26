#' Probability that at least m people share a birthday
#'
#' Internal helper for birthday problem demonstrations.
#'
#' @keywords internal
birthday_dp <- function(n, m){

    days <- 365

    dp <- rep(0, n + 1)
    dp[1] <- 1

    for(day in 1:days){

        new <- rep(0, n + 1)

        for(people in 0:n){

            if(dp[people + 1] > 0){

                remaining <- n - people

                probs <- dbinom(
                    0:remaining,
                    remaining,
                    1/(days-day+1)
                )

                for(add in 0:remaining){

                    if(add < m){

                        new[people + add + 1] <-
                            new[people + add + 1] +
                            dp[people + 1] *
                            probs[add + 1]

                    }
                }
            }
        }

        dp <- new
    }

    1 - sum(dp)

}

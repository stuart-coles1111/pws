#' Simulate goals in multiple games
#'
#' Simulates goals in multiple games as described in Chapter 1 of Playing With Statistics
#'
#' @param n_sim Number of games to simulate
#' @param pois_mean Average number of events per game
#' @param mu Average conversion probability
#' @param phi Concentration of conversion probabilities
#' @param plot_max Maximum value for subsequent plotting (optional)
#' @param seed Set seed to enable identical simulation across calls
#'
#' @returns  list containing number of simulated goals per game and a frequency table
#' @examples
#' goals_per_match <- goals_sim()
#' head(goals_per_match$data, 20)
#' barplot(Frequency ~ Goals, data = goals_per_match$table, col = "lightblue")
#'
#' @export
#'
goals_sim <- function(n_sim = 10000,
                      pois_mean = 25,
                      mu = 0.2,
                      phi = 2,
                      plot_max = 10,
                      seed = NULL) {
    if (!is.null(seed))
        set.seed(seed)
    res <- lapply(
        1:n_sim,
        pws:::game_goals_sim,
        pois_mean = pois_mean,
        mu = mu,
        phi = phi
    )  %>% unlist
    res_f <- factor(res, levels = 0:max(max(res), plot_max))
    res_df <- res_f %>% table %>% as.data.frame
    colnames(res_df) <- c("Goals", "Frequency")
    res_df$Goals <- as.numeric(levels(res_df$Goals))[res_df$Goals]
#    res_df$Goals <- factor(
#        res_df$Goals,
#        levels = 0:ifelse(
#            is.null(plot_max),
#            res_df$Goals %>% as.numeric %>% max,
#            plot_max
#        )
#    )
    list(data = res, table = res_df)
}

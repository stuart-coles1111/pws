#' Simulate goals in multiple games
#'
#' Simulates goals in multiple games as described in Chapter 1 of Playing With Statistics
#'
#' @param n_sim Number of games to simulate
#' @param pois_mean Average number of events per game
#' @param beta_alpha_1 First parameter that affects probability events become goals
#' @param beta_alpha_2 Second parameter that affects probability events become goals
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
                      beta_alpha_1 = 0.01,
                      beta_alpha_2 = 0.2,
                      plot_max = 10,
                      seed = 111) {
    if (!is.null(seed))
        set.seed(seed)
    res <- lapply(
        1:n_sim,
        pws:::game_goals_sim,
        pois_mean = pois_mean,
        beta_alpha_1 = beta_alpha_1,
        beta_alpha_2 = beta_alpha_2
    )  %>% unlist
    res_df <- res %>% table %>% as.data.frame
    colnames(res_df) <- c("Goals", "Frequency")
    res_df$Goals <- factor(
        res_df$Goals,
        levels = 0:ifelse(
            is.null(plot_max),
            res_df$Goals %>% as.numeric %>% max,
            plot_max
        )
    )
    list(data = res, table = res_df)
}

#' Quiz Answer Analysis
#'
#' Analyses response for a single question in Activity 5 of Playing With Statistics
#'
#' @param G best guess
#' @param S measure of accuracy
#' @param T true answer
#' @param alpha probability of interval
#' @param lines add lines to plots
#' @param final show plot for final score only (TRUE/FALSE)
#' @param dp decimal places for score
#'
#' @returns graphical analysis of score for quiz question
#' @examples
#' activity5_response_analysis(150, 20, activity5_quiz_answers$Answer[1], lines = T) # analysis of answer to question 1 of quiz
#'
#' @export
#'
#'
activity5_response_analysis <- function(G,
                             S,
                             T,
                             alpha = 0.95,
                             lines = FALSE,
                             final_score_only = FALSE,
                             dp = 2) {
    #get score information
    score <- pws::activity5_response_score(G, S, T, alpha, dp = dp)
    sigma <- score$sigma
    score <- score$scores

    #create dataframe for plotting points
    x <- seq(G - 3 * sigma, G + 3 * sigma, length = 1000)
    y <- dnorm(x, G, sigma)
    z <- log(y)
    m <- data.frame(x = x, y = y, z = z)
    paste0("Score is ", round(score, 2)) %>% print

    if (lines) {
        p1 <-
            ggplot2::ggplot(ggplot2::aes(x = x, y = y), data = m) +
            ggplot2::geom_line(colour = "indianred4") +
            ggplot2::geom_vline(xintercept = T, color = 'red') +
            ggplot2::xlab('T') + ggplot2::ylab('') +
            ggplot2::ggtitle('Normal Score') +
            pws:::shade_curve(m, G - S, G + S, fill = "lightblue") +
            ggplot2::xlim(min(x), max(x)) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                                         axis.title = ggplot2::element_text(size = 14)) +
            ggplot2::geom_hline(yintercept = exp(score), color = 'green')

        p2 <-
            ggplot2::ggplot(ggplot2::aes(x = x, y = z), data = m) +
            ggplot2::geom_line(colour = "indianred4") +
            ggplot2::geom_vline(xintercept = T, color = 'red') +
            ggplot2::xlab('T') +
            ggplot2::ylab('') +
            ggplot2::ggtitle(paste0('Final Score = ', round(score, 2))) +
            ggplot2::geom_hline(yintercept = score, color = 'green') +
            ggplot2::xlim(min(x), max(x)) + ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                                                                                           axis.title = ggplot2::element_text(size = 14))
    }

    else{
        p1 <-
            ggplot2::ggplot(ggplot2::aes(x = x, y = y), data = m) +
            ggplot2::geom_line(colour = "indianred4") +
            ggplot2::xlab('T') + ylab('') +
            ggplot2::ggtitle('Normal Score') +
            pws:::shade_curve(m, G - S, G + S, fill = "lightblue") +
            ggplot2::xlim(min(x), max(x)) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                  axis.title = ggplot2::element_text(size = 14))

        p2 <-
            ggplot2::ggplot(ggplot2::aes(x = x, y = z), data = m) +
            ggplot2::geom_line(colour = "indianred4") +
            ggplot2::xlab('T') +
            ggplot2::ylab('') +
            ggplot2::ggtitle(paste0('Final Score = ', round(score, 2))) +
            ggplot2::xlim(min(x), max(x)) +
            ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                                         axis.title = ggplot2::element_text(size = 14))

    }

    if (final_score_only) {
        p2
    }
    else{
        grid::grid.draw(rbind(ggplot2::ggplotGrob(p1), ggplot2::ggplotGrob(p2), size = "last"))
    }
}

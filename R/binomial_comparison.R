#' Comparsion of binomial distributions
#'
#' Compares probability distributions and simulated data for 2 binomial distributions
#'
#'
#' @param n Value of n for both binomial distributions
#' @param probs Vector of length 2 containing values of p for the 2 binomials
#' @param nsim Vector of length 2 specifying sample sizes for simulation
#' @param seed Value of seed for random number generator
#'
#' @returns  Graph of probability functions and bar charts of simulated data
#' @examples
#' binom_comparison()
#' binom_comparison(n = 30, probs = c(0.2, 0.5), nsim= c(25, 250))
#'
#' @export
#'

binom_comparison <- function(n = 20, probs = c(0.4, 0.1), nsim = c(100, 1000), seed = NA){
    width <- 0.1
    if(!is.na(seed)) set.seed(seed)
    pr1 <- dbinom(0:n, n, probs[1])
    pr2 <- dbinom(0:n, n, probs[2])
    m <- max(c(pr1, pr2))
    df <- data.frame(x = 0:n, y1 = pr1, y2 = pr2)

    g1 <- ggplot2::ggplot(df) +  ggplot2::geom_bar( ggplot2::aes(x, y1), stat="identity", fill="lightblue", width = width) +
        ggplot2::geom_point( ggplot2::aes(x, y1), colour="steelblue", size = 2) +  ggplot2::ylim(0, m) +  ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Probability")
    g1 <- g1 +  ggplot2::ggtitle(paste0("Expectation = ", n * probs[1], ", Standard Deviation = ", sqrt(n * probs[1] * (1 - probs[1]))%>% round(3))) +
        ggplot2::theme(plot.title =  ggplot2::element_text(size = 8, face = "bold"))

    g2 <-  ggplot2::ggplot(df) + ggplot2::geom_bar( ggplot2::aes(x, y2), stat="identity", fill="lightblue", width = width) +
        ggplot2::geom_point( ggplot2::aes(x, y2), colour="steelblue", size = 2) +  ggplot2::ylim(0, m) +  ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Probability")
    g2 <- g2 +  ggplot2::ggtitle(paste0("Expectation = ", n * probs[2], ", Standard Deviation = ", sqrt(n * probs[2] * (1 - probs[2]))%>% round(3)))  +
        ggplot2::theme(plot.title =  ggplot2::element_text(size = 8, face = "bold"))

    s1 <- rbinom(nsim[1], n, probs[1])
    s2 <- rbinom(nsim[1], n, probs[2])
    s1_tab <- table(factor(s1, levels = 0:n)) %>% as.data.frame
    s2_tab <- table(factor(s2, levels = 0:n)) %>% as.data.frame
    m <- max(c(s1_tab$Freq, s2_tab$Freq))

    g3 <-  ggplot2::ggplot(s1_tab,  ggplot2::aes(x = Var1, y = Freq)) +  ggplot2::geom_bar(stat = "identity", fill = "lightblue", width = 2 * width)  +
        ggplot2::scale_x_discrete(drop=FALSE, breaks = seq(0, n ,by=5), labels =  seq(0 , n,by = 5)) +  ggplot2::ylim(0, m) +  ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Frequency")

    g4 <-  ggplot2::ggplot(s2_tab,  ggplot2::aes(x = Var1, y = Freq)) +  ggplot2::geom_bar(stat = "identity", fill = "lightblue", width = 2 * width)  +
        ggplot2::scale_x_discrete(drop=FALSE, breaks = seq(0, n, by=5), labels =  seq(0, n, by = 5)) +  ggplot2::ylim(0, m) +  ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Frequency")

    g3 <- g3 +  ggplot2::ggtitle(paste0("Sample size = ", nsim[1], ", Mean = ", mean(s1)%>% round(3), ", SD = ", sd(s1)%>% round(3)))  +
        ggplot2::theme(plot.title =  ggplot2::element_text(size = 8, face = "bold"))
    g4 <- g4 +  ggplot2::ggtitle(paste0("Sample size = ", nsim[1], ", Mean = ", mean(s2)%>% round(3), ", SD = ", sd(s2)%>% round(3))) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 8,face = "bold"))


    s1 <- rbinom(nsim[2], n, probs[1])
    s2 <- rbinom(nsim[2], n, probs[2])
    s1_tab <- table(factor(s1, levels = 0:n)) %>% as.data.frame
    s2_tab <- table(factor(s2, levels = 0:n)) %>% as.data.frame
    m <- max(c(s1_tab$Freq, s2_tab$Freq))

    g5 <-  ggplot2::ggplot(s1_tab,  ggplot2::aes(x = Var1, y = Freq)) +  ggplot2::geom_bar(stat="identity", fill="lightblue", width=2*width)  +
        ggplot2::scale_x_discrete(drop=FALSE, breaks = seq(0, n, by = 5), labels =  seq(0 , n, by = 5)) +  ggplot2::ylim(0, m) +   ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Frequency")

    g6 <-  ggplot2::ggplot(s2_tab,  ggplot2::aes(x = Var1, y = Freq)) +  ggplot2::geom_bar(stat="identity", fill="lightblue", width=2*width)  +
        ggplot2::scale_x_discrete(drop=FALSE, breaks = seq(0, n, by = 5), labels =  seq(0, n, by = 5)) +  ggplot2::ylim(0, m) +  ggplot2::xlab("Number of Wins") +  ggplot2::ylab("Frequency")

    g5 <- g5 +  ggplot2::ggtitle(paste0("Sample size = ", nsim[2], ", Mean = ", mean(s1)%>% round(3), ", SD = ", sd(s1)%>% round(3)))  +
        ggplot2::theme(plot.title =  ggplot2::element_text(size=8, face = "bold"))
    g6 <- g6 +  ggplot2::ggtitle(paste0("Sample size = ", nsim[2], ", Mean = ", mean(s2)%>% round(3), ", SD = ", sd(s2)%>% round(3))) +
        ggplot2::theme(plot.title =  ggplot2::element_text(size=8, face = "bold"))

    gridExtra::grid.arrange(g1,g2,g3, g4, g5, g6, ncol = 2)
}


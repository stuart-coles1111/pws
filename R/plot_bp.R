#' THe Birthday Problem
#'
#' Probabiltiy calculations for the classical birthday problem
#'
#' @param nmax upper value for plotting
#'
#' @returns  graph of probabiltiys for the birthday problem
#' @examples
#' prop._bp()
#'
#' @export
#'

plot_bp <- function(nmax = 60) {
    bp <- function(n) {
        p <- 1
        if (n > 1) {
            for (i in 1:n)
                p <- p * (365 - i + 1) / 365
        }
        p <- 1 - p
        p
    }
    df <- cbind(N = 1:nmax, P = sapply(1:nmax, bp)) %>% data.frame
    ggplot(df, aes(N, P)) + geom_point(colour = "steelblue") + geom_hline(yintercept = 0.5, color = "red") +
        theme(axis.title = element_text(face = "italic"))
}


plot_bp <- function(nmax = 60, p_target = 0.5){

    bp <- function(n) {
        p <- 1
        if (n > 1) {
            for (i in 1:n)
                p <- p * (365 - i + 1) / 365
        }
        p <- 1 - p
        p
    }

    df <- data.frame(
        N = 1:nmax,
        P = sapply(1:nmax, bp)
    )

    required_n <- min(df$N[df$P >= p_target])

    p <- ggplot(df, aes(N,P)) +
        geom_point(colour="steelblue") +
        geom_hline(yintercept=p_target, colour="red") +
        geom_vline(xintercept=required_n,
                   colour="red",
                   linetype=2)

    list(
        plot = p,
        data = df,
        required_n = required_n,
        target_probability = p_target
    )
}



#' Plot a fitted xG
#'
#' Fits an xG model  to data simulated using the xGsim function
#'
#'
#' @param xG_data a list obtained from running xGsim
#' @param plotlim axis limit for plotting
#' @returns plots showing fitted xG values and data for simulated xG data
#' @examples
#' xG_data <- xGsim()
#' xGplot(xG_data)
#' @export
#'
#'
xGplot <- function(xG_data, plotlim = 60) {
    pl1 <- ggplot2::ggplot(subset(xG_data$data, body == "Foot")) +
        ggplot2::geom_point(ggplot2::aes(x, y, color = goal), size = 0.1) +
        ggplot2::xlim(-plotlim, plotlim) + ggplot2::ylim(0, plotlim) +
        ggplot2::ggtitle("Foot") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::xlab("x") +
        ggplot2::ylab("y") +
        ggplot2::theme(axis.title = ggplot2::element_text(face="italic"))

    pl2 <- ggplot2::ggplot(subset(xG_data$data, body == "Head")) +
        ggplot2::geom_point(ggplot2::aes(x, y, color = goal), size = 0.1) +
        ggplot2::xlim(-plotlim, plotlim) + ggplot2::ylim(0, plotlim) +
        ggplot2::ggtitle("Head") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::xlab("x") + ggplot2::ylab("y") +
        ggplot2::theme(axis.title = ggplot2::element_text(face="italic"))

    mod <- xGfit(xG_data)$model
    x <- seq(-plotlim, plotlim, length = 100)
    y <- seq(0, plotlim, length = 100)

    f <- function(x, y, body = "Foot") {
        angle <- ifelse(x == 0 & y == 0, 0, atan(x / y))
        angle_trans <- sin(angle * pi / 180)
        distance <- sqrt(x ^ 2 + y ^ 2)

        lp <-
            mod$coefficients["(Intercept)"] +
            mod$coefficients["bodyHead"] * (body == "Head") +
            mod$coefficients["angle_trans"] * angle_trans +
            mod$coefficients["distance"] * distance +
            mod$coefficients["bodyHead:distance"] * distance * (body == "Head")
        p <- exp(lp) / (1 + exp(lp))
        p
    }

    p <- outer(x, y, f, body = "Foot")
    p1 <- outer(x, y, function(x, y) x)
    p2 <- outer(x, y, function(x, y) y)


    m <- melt(p)
    m1 <- melt(p1)
    m2 <- melt(p2)

    m <- cbind(m1$value, m2$value, m$value)
    m <- as.data.frame(m)
    colnames(m) <- c('x', 'y', 'p')


    pl3 <- ggplot2::ggplot(m, ggplot2::aes(x = x, y = y, fill = p)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "white", high = "red")   +
        ggplot2::xlab("x") + ggplot2::ylab("y") +
        ggplot2::theme(axis.title = ggplot2::element_text(face="italic"))


    p <- outer(x, y, f, body = "Head")
    p11 <- outer(x, y, function(x, y) x)
    p2 <- outer(x, y, function(x, y) y)


    m <- melt(p)
    m1 <- melt(p1)
    m2 <- melt(p2)
    m <- cbind(m1$value, m2$value, m$value)
    m <- as.data.frame(m)
    colnames(m) <- c('x', 'y', 'p')


    pl4 <-  ggplot2::ggplot(m, ggplot2::aes(x = x, y = y, fill = p)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "white", high = "red")  +
        ggplot2:: xlab("x") + ggplot2::ylab("y") +
        ggplot2::theme(axis.title = ggplot2::element_text(face="italic"))

    grid.arrange(pl1, pl2, pl3, pl4, nrow = 2)
}

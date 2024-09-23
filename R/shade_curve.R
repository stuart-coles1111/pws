shade_curve <-
    function(MyDF,
             zstart,
             zend,
             fill = "red",
             alpha = .5) {
        ggplot2::geom_ribbon(
            data = subset(MyDF, x >= zstart & x <= (zend)),
            ggplot2::aes(ymin = 0, ymax = y),
            fill = fill,
            color = NA,
            alpha = alpha
        )
    }

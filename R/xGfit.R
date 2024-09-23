#' Fit an xG model to simulated data
#'
#' Fits an xG model  to data simulated using the xGsim function
#'
#'
#' @param xG_data a list obtained from running xGsim


#' @returns  List comprising
#' model: the fitted model. pars: the parameters used in simulating the data.
#' summary: a comparison of true and estimated parameters
#' @examples
#' xG_data <- xGsim()
#' xGfit(xG_data)
#' @export
#'
xGfit <- function(xG_data) {
    xG_data$data$angle_trans <- sin(xG_data$data$angle * pi / 180)
    mod <-
        glm(goal ~ body * distance + angle_trans,
            family = binomial,
            data = xG_data$data)

    h <- summary(mod)$cov.unscaled
    #    se <- h %>% diag %>% sqrt    .... calculates standard errors... not used here

    summary_mat <-
        data.frame(pars = xG_data$pars, ests = mod$coefficients)
    list(model = mod, summary = summary_mat)
}

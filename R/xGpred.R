#' Predict xG
#'
#' Predicts xG according to model with specified parameters
#'
#'
#' @param xG_mod fitted xG model
#' @param x vector of x-coordinates of events
#' @param y vector of y-coordinates of events
#' @param body character vector of elements ("Foot"/"Head") indicating type of shot per event

#' @returns predicted xG and event info
#' @examples
#' xG_data <- xGsim()
#' xG_model <- xGfit(xG_data)
#' xGpred(xG_model, x= c(5, 15, -20, 7, -12),
#'         y = c(10, 18, 6, 14, 25),
#'         body = c("Head", "Head",
#'             "Foot", "Foot", "Foot"))
#'
#' @export
#'
xGpred <- function(xG_mod, x, y, body) {
    angle <- ifelse(x == 0 & y == 0, 0, atan(x / y))
    angle_trans <- sin(angle * pi / 180)
    distance <- sqrt(x ^ 2 + y ^ 2)
    newdata <- data.frame(distance = distance,
                          angle_trans = angle_trans,
                          body = body)
    predict(xG_mod$model, newdata = newdata, type = "response")
}

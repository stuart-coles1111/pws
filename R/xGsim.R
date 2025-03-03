#' Simulate xG
#'
#' Simulates xG according to model with specified parameters from a specified number of scoring opportunities
#'
#'
#' @param n_data Number of events to simulate
#' @param bodypar parameter determining rate of chances with Head/Foot respectively
#' @param distpar paramter determining distribution of distances of shots
#' @param anglepar paramter determining distribution of angles of shots
#' @param intercept intercept in linear predictor part of model
#' @param dist_coeff coefficient of distance in linear predictor part of model
#' @param dist_body_inter interaction term for body type and distance in linear predictor part of model
#' @param body_coeff coefficient of body type in linear predictor part of model
#' @param angle_coeff coefficient of angle in linear predictor part of model

#' @returns  List comprising
#' data: the simulated data
#' pars: the model parameters used in simulating the model
#' @examples
#' xGsim()
#'
#' @export
#'
xGsim <-
    function(n_data,
             bodypar = 0.2,
             distpar = 0.2,
             anglepar = 3,
             intercept = 0.25,
             dist_coeff = -0.1,
             dist_body_inter = -0.25,
             body_coeff = -0.05,
             angle_coeff = -0.8,
             seed = NULL) {
        if (!is.null(seed))
            set.seed(seed)
        body <-
            sample(
                c("Head", "Foot"),
                n_data,
                replace = T,
                prob = c(bodypar, 1 - bodypar)
            )
        distance <- rexp(n_data, distpar) %>% round(3)
        angle <- 180 * (rbeta(n_data, anglepar, anglepar) - .5) %>% round(3)
        lp <-
            intercept +  dist_coeff * distance + dist_body_inter * distance * (body ==
                                                                                   "Head") + body_coeff * (body == "Head") + angle_coeff * sin(angle * pi /
                                                                                                                                                   180)
        p <- (exp(lp) / (1 + exp(lp))) %>% round(3)
        goal <- rbinom(n_data, 1, p)
        df <- data.frame(
            distance = distance,
            angle = angle,
            body = body,
            prob_true = p,
            goal = goal
        )
        df$x <- (distance * sin(df$angle * pi / 180)) %>% round(3)
        df$y <- (distance * cos(df$angle * pi / 180)) %>% round(3)
        df <-
            df[, c("x", "y", "distance", "angle", "body", "prob_true", "goal")]
        df$goal <- factor(df$goal)
        list(
            data = df,
            pars = c(
                intercept,
                body_coeff,
                dist_coeff,
                angle_coeff,
                dist_body_inter
            )
        )
    }

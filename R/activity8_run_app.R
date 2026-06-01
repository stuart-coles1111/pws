#' Run Activity 8 app
#' @export
activity8_run_app <- function(
        commission_max = 0.2,
        bet_rate = 5,
        bet_proportion = 0.1,
        pool_init = 100,
        nominal_stake = 100
) {

    appDir <- system.file(
        "shiny/app_activity8_dev2.R",
        package = "pws"
    )

    env <- new.env(parent = globalenv())

    env$commission_max <- commission_max
    env$bet_rate <- bet_rate
    env$bet_proportion <- bet_proportion
    env$pool_init <- pool_init
    env$nominal_stake <- nominal_stake

    # source app
    source(appDir, local = env)

    # 🚨 THIS forces launch
    shiny::runApp(
        list(
            ui = env$ui,
            server = env$server
        )
    )
}

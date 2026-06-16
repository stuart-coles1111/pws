detach("package:pws", unload = TRUE)
remotes::install_github("stuart-coles1111/pws",force=TRUE)

rsconnect::deployApp(
    appDir = "inst/posit/stats_lab_hub",
    appName = "hub_app"
)

rsconnect::deployApp(
    appDir = "inst/shiny/stats_lab",
    appName = "stats_lab"
)

rsconnect::deployApp(
    appDir = "inst/shiny/app_activity1",
    appName = "app-activity-1"
)


https://bujx5j-stuart-coles.shinyapps.io/hub_app/

safe_launch <- function(app_name) {

    tryCatch({

        launch_activity(app_name)

    }, error = function(e) {

        message(
            "\nUnable to start graphical app.\n",
            "Opening console menu instead.\n"
        )

        stats_lab()
    })
}

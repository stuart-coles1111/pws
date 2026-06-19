library(shiny)
library(bslib)

ui <- page_sidebar(

    title = "Apps for Playing With Statistics",

    sidebar = sidebar(

        div(
            style = "
            padding:20px;
            border-radius:14px;
            background:linear-gradient(90deg,#A8DADC,#CDB4DB);
            margin-bottom:20px;
            ",

            h2(
                "📊 Statistics Playground",
                style = "
                font-weight:700;
                color:#2c3e50;
                "
            ),

            p(
                "Tools, investigations and activity apps to support Playing With Statistics",
                style = "
                color:#34495e;
                "
            )
        ),

        hr(),

        actionButton(
            "quit",
            "Exit",
            class = "btn-danger",
            width = "100%"
        )
    ),


    div(

        style = "
        padding:20px;
        ",

        div(
            style = "
            padding:20px;
            border-radius:14px;
            background:linear-gradient(90deg,#A8DADC,#CDB4DB);
            margin-bottom:25px;
            ",

            h1(
                "🎲 Apps for Playing With Statistics",
                style="
                font-weight:700;
                color:#2c3e50;
                "
            )
        ),


        layout_column_wrap(

            width = 1/3,


            card(

                style="
                border-radius:16px;
                border:none;
                box-shadow:0 4px 12px rgba(0,0,0,0.08);
                ",

                card_header(
                    "📈 Statistics Lab"
                ),

                p(
                    "Chapter by chapter investigations."
                ),

                actionButton(
                    "lab",
                    "Open Lab",
                    class="btn-primary"
                )
            ),


            card(
                style="
                border-radius:16px;
                border:none;
                box-shadow:0 4px 12px rgba(0,0,0,0.08);
                ",

                card_header(
                    "🎯 Activities"
                ),

                p(
                    "Activities for first half of book."
                ),

                actionButton(
                    "activity1",
                    "Activity 1",
                    class="btn-primary"
                ),

                actionButton(
                    "activity2",
                    "Activity 2",
                    class="btn-primary"
                ),

                actionButton(
                    "activity3",
                    "Activity 3",
                    class="btn-primary"
                ),

                actionButton(
                    "activity4",
                    "Activity 4",
                    class="btn-primary"
                )
            ),


            card(
                style="
                border-radius:16px;
                border:none;
                box-shadow:0 4px12px rgba(0,0,0,0.08);
                ",

                card_header(
                    "🔍 More Activities"
                ),

                p(
                    "Activities for second half of book."
                ),


                actionButton(
                    "activity5",
                    "Activity 5",
                    class="btn-primary"
                ),

                actionButton(
                    "activity6",
                    "Activity 6",
                    class="btn-primary"
                ),

                actionButton(
                    "activity7",
                    "Activity 7",
                    class="btn-primary"
                ),

                actionButton(
                    "activity8",
                    "Activity 8",
                    class="btn-primary"
                )
            )

        )
    )
)

server <- function(input, output, session) {

    observeEvent(input$lab, {
        launch_activity("stats_lab")
    })

    observeEvent(input$activity1, {
        launch_activity("app_activity1")
    })

    observeEvent(input$activity2, {
        launch_activity("app_activity2")
    })

    observeEvent(input$activity3, {
        launch_activity("app_activity3")
    })

    observeEvent(input$activity4, {
        launch_activity("app_activity4")
    })

    observeEvent(input$activity5, {
        launch_activity("app_activity5")
    })

    observeEvent(input$activity6, {
        launch_activity("app_activity6")
    })

    observeEvent(input$activity7, {
        launch_activity("app_activity7")
    })

    observeEvent(input$activity8, {
        launch_activity("app_activity8")
    })

    observeEvent(input$quit, {
        stopApp()
    })

}

shinyApp(ui, server)

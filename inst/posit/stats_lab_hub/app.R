library(shiny)

lab_url <- "https://bujx5j-stuart-coles.shinyapps.io/stats_lab"
chapter1_url <- "https://bujx5j-stuart-coles.shinyapps.io/app-activity-1"
chapter2_url <- "https://bujx5j-stuart-coles.shinyapps.io/app-activity-2"
chapter3_url <- "https://bujx5j-stuart-coles.shinyapps.io/app-activity-3"

# placeholders for future chapters
coming_soon_url <- "#"


ui <- fluidPage(

    tags$head(
        tags$style(HTML("
            body {
                background: #f5f7fb;
            }

            h3 {
                margin-bottom: 6px;
                color: #1f2d3d;
            }

            p {
                color: #5b6775;
                margin-top: 6px;
            }

            .section-card {
                background: white;
                padding: 18px 22px;
                border-radius: 14px;
                box-shadow: 0 6px 18px rgba(0,0,0,0.06);
                margin-bottom: 22px;
                transition: transform 0.15s ease, box-shadow 0.15s ease;
            }

            .section-card:hover {
                transform: translateY(-2px);
                box-shadow: 0 10px 22px rgba(0,0,0,0.08);
            }

            hr {
                border: none;
                height: 1px;
                background: #e6e9ef;
                margin: 40px 0;
            }

            a.button-link {
                display: inline-block;
                padding: 12px 18px;
                color: white;
                border-radius: 8px;
                text-decoration: none;
                font-weight: 600;
                transition: all 0.15s ease;
            }

            a.button-link:hover {
                opacity: 0.9;
                transform: translateY(-1px);
            }

            a.disabled-link {
                display: inline-block;
                padding: 12px 18px;
                background: #d0d5dd;
                color: #6b7280;
                border-radius: 8px;
                text-decoration: none;
                font-weight: 600;
                cursor: not-allowed;
            }
        "))
    ),

    titlePanel("Apps for Playing With Statistics"),

    div(
        style = "margin-top: 30px;",

        # Lab
        div(class = "section-card",
            tags$h3("Statistics Lab"),
            tags$p("Investigations for Playing With Statistics"),
            tags$a("▶ Start Lab",
                   href = lab_url,
                   target = "_blank",
                   class = "button-link",
                   style = "background:#2C7FB8;"
            )
        ),

        # Chapter 1
        div(class = "section-card",
            tags$h3("Activity 1: Picturing Randomness"),
            tags$p("Explore randomness using simulations and visualisations."),
            tags$a("▶ Start Activity 1",
                   href = chapter1_url,
                   target = "_blank",
                   class = "button-link",
                   style = "background:#3A86FF;"
            )
        ),

        # Chapter 2
        div(class = "section-card",
            tags$h3("Activity 2: Who Wants to be a Danish Millionaire?"),
            tags$p("Work with distributions, summaries, and basic statistical descriptions."),
            tags$a("▶ Start Activity 2",
                   href = chapter2_url,
                   target = "_blank",
                   class = "button-link",
                   style = "background:#2A9D8F;"
            )
        ),

        # Chapter 3
        div(class = "section-card",
            tags$h3("Activity 3: Place Your Bets"),
            tags$p("Deciding bets and stake sizes"),
            tags$a("▶ Start Activity 3",
                   href = chapter3_url,
                   target = "_blank",
                   class = "button-link",
                   style = "background:#E76F51;"
            )
        ),

        # Chapter 4 (coming soon)
        div(class = "section-card",
            tags$h3("Activity 4: Coming Soon"),
            tags$p("This activity will be released later."),
            tags$a("Coming soon",
                   href = coming_soon_url,
                   class = "disabled-link"
            )
        ),

        # Chapter 5
        div(class = "section-card",
            tags$h3("Activity 5: Coming Soon"),
            tags$p("This activity will be released later."),
            tags$a("Coming soon",
                   href = coming_soon_url,
                   class = "disabled-link"
            )
        ),

        # Chapter 6
        div(class = "section-card",
            tags$h3("Activity 6: Coming Soon"),
            tags$p("This activity will be released later."),
            tags$a("Coming soon",
                   href = coming_soon_url,
                   class = "disabled-link"
            )
        ),

        # Chapter 7
        div(class = "section-card",
            tags$h3("Activity 7: Coming Soon"),
            tags$p("This activity will be released later."),
            tags$a("Coming soon",
                   href = coming_soon_url,
                   class = "disabled-link"
            )
        ),

        # Chapter 8
        div(class = "section-card",
            tags$h3("Activity 8: Coming Soon"),
            tags$p("This activity will be released later."),
            tags$a("Coming soon",
                   href = coming_soon_url,
                   class = "disabled-link"
            )
        )
    )
)

server <- function(input, output, session) {}

shinyApp(ui, server)

library(shiny)

ui <- fluidPage(
    verbatimTextOutput("txt")
)

server <- function(input, output, session) {
    rv <- reactiveValues(one = 1, two = 2, three = 3)

    output$txt <- renderPrint({
        vals <- list()
        for (val in reactiveValuesToList(rv)) {
            vals[[val]] <- val
        }
        for (i in seq_len(length(vals))) {
            print(vals[[i]])
        }
    })
}


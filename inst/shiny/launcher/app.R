library(shiny)

ui <- fluidPage(

    titlePanel("Apps for Playing With Statistics"),

    br(),

    actionButton(
        "lab",
        "▶ Statistics Lab"
    ),

    br(), br(),

    actionButton(
        "activity1",
        "▶ Activity 1"
    ),

    br(), br(),

    actionButton(
        "activity2",
        "▶ Activity 2"
    ),

    br(), br(),

    actionButton(
        "activity3",
        "▶ Activity 3"
    ),

    br(), br(),

    actionButton(
        "activity4",
        "▶ Activity 4"
    ),

    br(), br(),

    actionButton(
        "activity5",
        "▶ Activity 5"
    ),

    br(), br(),

    actionButton(
        "activity6",
        "▶ Activity 6"
    ),

    br(), br(),

    actionButton(
        "activity7",
        "▶ Activity 7"
    ),

    br(), br(),

    actionButton(
        "activity8",
        "▶ Activity 8"
    ),

    br(), br(),

    actionButton(
        "quit",
        "Exit")
)

server <- function(input, output, session) {

    observeEvent(input$lab, {
        stopApp("lab")
    })

    observeEvent(input$activity1, {
        stopApp("activity1")
    })

    observeEvent(input$activity2, {
        stopApp("activity2")
    })

    observeEvent(input$activity3, {
        stopApp("activity3")
    })

    observeEvent(input$activity4, {
        stopApp("activity4")
    })

    observeEvent(input$activity5, {
        stopApp("activity5")
    })

    observeEvent(input$activity6, {
        stopApp("activity6")
    })

    observeEvent(input$activity7, {
        stopApp("activity7")
    })

    observeEvent(input$activity8, {
        stopApp("activity8")
    })

    observeEvent(input$quit, {
        stopApp("quit")
    })

}

shinyApp(ui, server)

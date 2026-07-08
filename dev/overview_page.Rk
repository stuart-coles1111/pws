#' Create a standard Overview tab for an activity app
#'
#' Creates a Shiny `nav_panel()` containing the standard overview layout
#' used throughout the activity apps. The overview consists of four
#' sections: *Activity Explanation*, *Individual Execution*,
#' *Group Execution*, and *Question to Explore*.
#'
#' @param title A character string giving the title displayed at the top of
#'   the overview page.
#' @param explanation UI content describing the purpose and statistical
#'   background of the activity.
#' @param individual UI content describing how the activity should be
#'   completed individually.
#' @param group UI content describing how the activity should be completed
#'   in groups or as a class.
#' @param question UI content containing the key question(s) students should
#'   investigate during the activity.
#'
#' @return A `bslib::nav_panel()` object that can be supplied as a child of
#'   `bslib::page_navbar()`.
#'
#' @export
#'
#' @examples
#' overview_page(
#'   title = "Activity 1: Picturing Randomness",
#'   explanation = tagList(
#'     p("Students investigate how random sequences behave.")
#'   ),
#'   individual = tagList(
#'     tags$ol(
#'       tags$li("Complete the activity individually."),
#'       tags$li("Compare your results with theory.")
#'     )
#'   ),
#'   group = tagList(
#'     p("Pool class results and discuss any differences.")
#'   ),
#'   question = tagList(
#'     p("Do humans generate truly random sequences?")
#'   )
#' )

overview_page <- function(
        title = "Overview",
        explanation,
        individual,
        group,
        question
) {

    nav_panel(

        title,

        # =====================================================
        # EXPLANATION
        # =====================================================

        bslib::card(
            full_screen = FALSE,
            class = "overview-card",

            bslib::card_header(
                h3("📌 Activity Overview")
            ),

            bslib::card_body(
                explanation
            )
        ),

        br(),

        # =====================================================
        # INDIVIDUAL + GROUP
        # =====================================================

        fluidRow(

            column(
                width = 6,

                bslib::card(
                    class = "overview-card",

                    bslib::card_header(
                        h3("🧑 Individual implementation")
                    ),

                    bslib::card_body(
                        individual
                    )
                )
            ),

            column(
                width = 6,

                bslib::card(
                    class = "overview-card",

                    bslib::card_header(
                        h3("👥 Group implementation")
                    ),

                    bslib::card_body(
                        group
                    )
                )
            )
        ),

        br(),

        # =====================================================
        # QUESTION
        # =====================================================

        bslib::card(
            class = "overview-card question-card",

            bslib::card_header(
                h3("❓ Questions to consider"),
            ),

            bslib::card_body(
                question
            )
        )
    )
}

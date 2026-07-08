#' Create a standard Overview tab for an activity app
#'
#' Creates a Shiny `nav_panel()` containing the standard overview layout
#' used throughout the activity apps. The overview consists of four
#' sections: Activity Explanation, Individual Execution,
#' Group Execution, and Question to Explore.
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

overview_page <- function(
        title = "Overview",
        explanation,
        individual,
        group,
        question
) {

    nav_panel(

        title,

        div(

            style = "
                height: calc(100vh - 170px);
                display: flex;
                flex-direction: column;
                gap: 15px;
            ",


            # =====================================================
            # EXPLANATION (35%)
            # =====================================================

            bslib::card(

                class = "overview-card",

                style = "
                    flex: 0 0 35%;
                    overflow: hidden;
                ",

                bslib::card_header(
                    h3("📌 Activity Overview")
                ),

                bslib::card_body(

                    style = "
                        overflow-y: auto;
                    ",

                    explanation
                )
            ),


            # =====================================================
            # INDIVIDUAL + GROUP (45%)
            # =====================================================

            div(

                style = "
        flex: 0 0 45%;
        min-height:0;
        display:flex;
        gap:15px;
    ",


                # -----------------------------
                # Individual
                # -----------------------------

                div(

                    style = "
            flex:1;
            min-width:0;
            min-height:0;
        ",


                    bslib::card(

                        class = "overview-card",

                        style = "
                height:100%;
                display:flex;
                flex-direction:column;
                overflow:hidden;
            ",


                        bslib::card_header(
                            h3("🧑 Individual implementation")
                        ),


                        bslib::card_body(

                            style = "
                    flex:1;
                    min-height:0;
                    overflow-y:auto;
                ",

                            individual
                        )
                    )
                ),


                # -----------------------------
                # Group
                # -----------------------------

                div(

                    style = "
            flex:1;
            min-width:0;
            min-height:0;
        ",


                    bslib::card(

                        class = "overview-card",

                        style = "
                height:100%;
                display:flex;
                flex-direction:column;
                overflow:hidden;
            ",


                        bslib::card_header(
                            h3("👥 Group implementation")
                        ),


                        bslib::card_body(

                            style = "
                    flex:1;
                    min-height:0;
                    overflow-y:auto;
                ",

                            group
                        )
                    )
                )
            ),

            # =====================================================
            # QUESTIONS (20%)
            # =====================================================

            bslib::card(

                class = "overview-card question-card",

                style = "
                    flex: 0 0 20%;
                    overflow:hidden;
                ",


                bslib::card_header(

                    h3("❓ Questions to consider")

                ),


                bslib::card_body(

                    style = "
                        overflow-y:auto;
                    ",

                    question
                )
            )
        )
    )
}

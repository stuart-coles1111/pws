chapter_page_ui <- function(
        id,
        title,
        sidebar,
        overview,
        code,
        results,
        learn,
        activity = NULL
){

    ns <- NS(id)

    page_sidebar(

        sidebar = sidebar,

        tagList(

            div(
                style = "
          padding:20px;
          border-radius:14px;
          background:linear-gradient(90deg,#A8DADC,#CDB4DB);
          margin-bottom:20px;
        ",

                h1(title)
            ),

            navset_card_tab(

                id = ns("chapter_tab"),

                nav_panel(
                    "Overview",
                    overview
                ),

                nav_panel(
                    "Investigate",
                    results
                ),

                nav_panel(
                    "Generated Code",
                    code
                ),

                nav_panel(
                    "Summary",
                    learn
                )
            )
        )
    )
}

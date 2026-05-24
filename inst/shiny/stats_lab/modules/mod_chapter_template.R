chapter_page_ui <- function(
        id,
        title,
        sidebar,
        overview,
        code,
        results,
        learn
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

                nav_panel(
                    "Overview",
                    overview
                ),

                nav_panel(
                    "Generated Code",
                    code
                ),

                nav_panel(
                    "Results",
                    results
                ),

                nav_panel(
                    "Learn",
                    learn
                )
            )
        )
    )
}

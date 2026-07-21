# =========================================================
# Statistics Lab (pws)
# =========================================================

suppressPackageStartupMessages({
library(shiny)
library(bslib)
library(pws)
library(dipsaus)
library(dplyr)
library(DT)
library(ggplot2)
library(grid)
library(gridExtra)
library(gt)
library(gtools)
library(later)
library(latex2exp)
library(magrittr)
library(MASS)
library(patchwork)
library(purrr)
library(reshape2)
library(scales)
library(shinyjs)
library(stringi)
library(tidyr)
})

# =========================================================
# Source reusable UI template
# =========================================================

source("modules/mod_chapter_template.R")

# =========================================================
# Source chapters
# =========================================================

source("chapters/toolkit.R")
source("chapters/chapter1.R")
source("chapters/chapter2.R")
source("chapters/chapter3.R")
source("chapters/chapter4.R")
source("chapters/chapter5.R")
source("chapters/chapter6.R")
source("chapters/chapter7.R")
source("chapters/chapter8.R")


# =========================================================
# UI
# =========================================================

ui <- page_navbar(

    title = "📘 Statistics Lab",

    header = shinyjs::useShinyjs(),

    theme = bs_theme(
        version = 5,
        bootswatch = "minty",
        primary = "#7B9ACC",
        secondary = "#CDB4DB",
        bg = "#F7F7FB",
        fg = "#2E3440"
    ),

    # =======================================================
    # Toolkit
    # =======================================================

    nav_panel(
        "🧰 Toolkit",
        stats_toolkit_ui("toolkit")
    ),


    # =======================================================
    # Chapter 1
    # =======================================================

    nav_panel(
        "🎲 Chapter 1",
        chapter1_ui("chapter1")
    ),

    # =======================================================
    # Chapter 2
    # =======================================================

    nav_panel(
        "🌗 Chapter 2",
        chapter2_ui("chapter2")
    ),

    # =======================================================
    # Chapter 3
    # =======================================================

    nav_panel(
        "⚖️  Chapter 3",
        chapter3_ui("chapter3")
    ),

    # =======================================================
    # Chapter 4
    # =======================================================

    nav_panel(
        "📉 Chapter 4",
        chapter4_ui("chapter4")
    ),

    # =======================================================
    # Chapter 5
    # =======================================================

    nav_panel(
        "📊 Chapter 5",
        chapter5_ui("chapter5")
    ),

    # =======================================================
    # Chapter 6
    # =======================================================

    nav_panel(
        "📐 Chapter 6",
        chapter6_ui("chapter6")
    ),

    # =======================================================
    # Chapter 7
    # =======================================================

    nav_panel(
        "🧩 Chapter 7",
        chapter7_ui("chapter7")
    ),

    nav_panel(
        "🕸 Chapter 8",
        chapter8_ui("chapter8")
    )
)




# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){

    # toolkit
    stats_toolkit_server("toolkit")

    # Chapter 1
    chapter1_server("chapter1")

    # Chapter 2
    chapter2_server("chapter2")

    # Chapter 3
    chapter3_server("chapter3")

    # Chapter 4
    chapter4_server("chapter4")

    # Chapter 5
    chapter5_server("chapter5")

    # Chapter 6
    chapter6_server("chapter6")

    # Chapter 7
    chapter7_server("chapter7")

    # Chapter 8
    chapter8_server("chapter8")
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)

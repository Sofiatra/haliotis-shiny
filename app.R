library(shiny)
library(tidyverse)
library(ggvenn)
library(DT)
library(plotly)
library(rmarkdown)
library(shinythemes)
library(bslib)
library(RColorBrewer)
library(colourpicker)
library(corrplot)

# ---- Module loading ----
source("modules/import_module.R")
source("modules/data_overview.R")
source("modules/data_transformation.R")
source("modules/filtering_module.R")
source("modules/plotting_module.R")
source("modules/results_display.R")
source("modules/report_module.R")

ui <- navbarPage(
  title = "Haliotis Data Analysis",
  id = "nav",
  theme = bs_theme(bootswatch = "flatly"),

  tabPanel(
    "1. Import / Overview",
    icon = icon("upload"),
    import_module_js(),
    import_module_ui("import")
  ),

  tabPanel(
    "2. Data overview",
    icon = icon("database"),
    data_overview_ui("overview")
  ),

  tabPanel(
    "3. Transformation & filtering",
    icon = icon("sliders-h"),
    fluidRow(
      column(
        12,
        data_transformation_ui("transformation"),
        filtering_module_ui("filtering")
      )
    )
  ),

  tabPanel(
    "4. Visualizations",
    icon = icon("chart-bar"),
    plotting_module_ui("plotting")
  ),

  tabPanel(
    "5. Statistical results",
    icon = icon("chart-line"),
    results_display_ui("results")
  ),

  tabPanel(
    "6. Report",
    icon = icon("file-alt"),
    report_module_ui("report")
  )
)

server <- function(input, output, session) {
  imported <- import_module_server("import")
  raw_data <- imported$data

  data_overview_server("overview", raw_data)

  transformed_data <- data_transformation_server(
    "transformation",
    raw_data
  )

  filtered_data <- filtering_module_server(
    "filtering",
    transformed_data
  )

  plotting_module_server(
    "plotting",
    raw_data,
    transformed_data
  )

  results_display_server(
    "results",
    list(
      raw = raw_data,
      transformed = transformed_data,
      filtered = filtered_data
    )
  )

  report_module_server(
    "report",
    list(
      raw = raw_data,
      transformed = transformed_data,
      filtered = filtered_data
    )
  )
}

shinyApp(ui, server)

data_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Haliotis dataset overview"),
    fluidRow(
      column(3, uiOutput(ns("n_abalone_box"))),
      column(3, uiOutput(ns("avg_rings_box"))),
      column(3, uiOutput(ns("avg_weight_box"))),
      column(3, uiOutput(ns("sex_ratio_box")))
    ),
    br(),
    wellPanel(
      h4("Dataset dimensions"),
      verbatimTextOutput(ns("dimensions")),
      h4("Column types"),
      DTOutput(ns("column_types")),
      h4("Missing values"),
      verbatimTextOutput(ns("na_check"))
    )
  )
}

data_overview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$n_abalone_box <- renderUI({
      req(data())
      div(class = "well", h4(nrow(data())), p("Observations"))
    })

    output$avg_rings_box <- renderUI({
      req(data())
      div(class = "well", h4(round(mean(data()$Rings, na.rm = TRUE), 1)), p("Mean rings"))
    })

    output$avg_weight_box <- renderUI({
      req(data())
      div(class = "well", h4(paste0(round(mean(data()$Whole.weight, na.rm = TRUE), 2), " g")), p("Mean whole weight"))
    })

    output$sex_ratio_box <- renderUI({
      req(data())
      sex_count <- table(data()$Sex)
      dominant_sex <- names(which.max(sex_count))
      div(class = "well", h4(dominant_sex), p("Most frequent sex category"))
    })

    output$dimensions <- renderPrint({
      req(data())
      cat(dim(data())[1], "rows ×", dim(data())[2], "columns\n")
    })

    output$column_types <- renderDT({
      req(data())
      tibble(
        Column = names(data()),
        Type = purrr::map_chr(data(), ~ class(.x)[1]),
        Unique = purrr::map_int(data(), ~ length(unique(.x)))
      ) |>
        datatable(options = list(dom = "t", scrollX = TRUE))
    })

    output$na_check <- renderPrint({
      req(data())
      na_count <- colSums(is.na(data()))
      if (sum(na_count) == 0) {
        cat("No missing values detected.\n")
      } else {
        print(na_count[na_count > 0])
      }
    })
  })
}

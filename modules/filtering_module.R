filtering_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        6,
        h3("Column selection"),
        wellPanel(
          h4("Morphometric columns"),
          DTOutput(ns("length_height_cols")),
          h4("Weight columns"),
          DTOutput(ns("weight_cols")),
          h4("All columns except Sex"),
          DTOutput(ns("no_sex_cols"))
        )
      ),
      column(
        6,
        h3("Example row filtering"),
        wellPanel(
          h4("Females with Diameter < 0.3"),
          DTOutput(ns("filtered_females")),
          h4("Older males and females"),
          DTOutput(ns("filtered_old"))
        )
      )
    )
  )
}

filtering_module_server <- function(id, transformed_data) {
  moduleServer(id, function(input, output, session) {
    output$length_height_cols <- renderDT({
      req(transformed_data())
      datatable(transformed_data() |> select(Length:Height), options = list(pageLength = 5))
    })

    output$weight_cols <- renderDT({
      req(transformed_data())
      datatable(transformed_data() |> select(contains("weight")), options = list(pageLength = 5))
    })

    output$no_sex_cols <- renderDT({
      req(transformed_data())
      datatable(transformed_data() |> select(-Sex), options = list(pageLength = 5))
    })

    output$filtered_females <- renderDT({
      req(transformed_data())
      datatable(
        transformed_data() |> filter(Sex == "F", Diameter < 0.3),
        options = list(pageLength = 5)
      )
    })

    output$filtered_old <- renderDT({
      req(transformed_data())
      datatable(
        transformed_data() |> filter(Age == "old", Sex %in% c("M", "F")),
        options = list(pageLength = 5)
      )
    })

    transformed_data
  })
}

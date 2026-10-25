data_transformation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Data transformation"),
    wellPanel(
      h4("Derived age category"),
      verbatimTextOutput(ns("age_definition")),
      h4("Observations by age category"),
      tableOutput(ns("age_count")),
      h4("Viscera weight by sex and age"),
      plotOutput(ns("stripchart_plot")),
      h4("Ordered sex factor"),
      plotOutput(ns("reordered_plot"))
    )
  )
}

data_transformation_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    transformed_data <- reactive({
      req(data())
      data() |>
        dplyr::mutate(
          Age = dplyr::case_when(
            Rings >= 1 & Rings <= 5 ~ "young",
            Rings >= 6 & Rings <= 13 ~ "adult",
            Rings >= 14 & Rings <= 30 ~ "old",
            TRUE ~ NA_character_
          ),
          Age = factor(Age, levels = c("young", "adult", "old")),
          Sex_ordered = factor(Sex, levels = c("I", "F", "M"))
        )
    })

    output$age_definition <- renderPrint({
      cat("Age is derived from Rings:\n")
      cat("- young: 1–5 rings\n")
      cat("- adult: 6–13 rings\n")
      cat("- old: 14–30 rings\n")
    })

    output$age_count <- renderTable({
      req(transformed_data())
      transformed_data() |>
        count(Age) |>
        rename(`Age category` = Age, Count = n)
    })

    output$stripchart_plot <- renderPlot({
      req(transformed_data())
      ggplot(transformed_data(), aes(x = Sex, y = Viscera.weight, color = Sex)) +
        geom_jitter(alpha = 0.7, width = 0.2) +
        facet_wrap(~ Age, scales = "free_y") +
        theme_minimal() +
        labs(x = "Sex", y = "Viscera weight (g)")
    })

    output$reordered_plot <- renderPlot({
      req(transformed_data())
      ggplot(transformed_data(), aes(x = Sex_ordered, y = Viscera.weight, color = Sex_ordered)) +
        geom_jitter(alpha = 0.7, width = 0.2) +
        facet_wrap(~ Age, scales = "free_y") +
        theme_minimal() +
        labs(x = "Sex (ordered)", y = "Viscera weight (g)")
    })

    transformed_data
  })
}

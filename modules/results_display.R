results_display_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel(
        "Summary",
        h4("Descriptive statistics"),
        verbatimTextOutput(ns("complete_summary")),
        h4("Correlation matrix"),
        plotOutput(ns("correlation_matrix"))
      ),
      tabPanel(
        "Interactive analysis",
        selectInput(ns("x_var"), "X variable:", choices = NULL),
        selectInput(ns("y_var"), "Y variable:", choices = NULL),
        selectInput(ns("color_var"), "Group variable:", choices = c("Sex", "Age")),
        plotlyOutput(ns("interactive_plot")),
        verbatimTextOutput(ns("correlation_test")),
        verbatimTextOutput(ns("group_comparison"))
      ),
      tabPanel(
        "Filtered data",
        DTOutput(ns("filtered_data_table")),
        downloadButton(ns("download_data"), "Download CSV")
      )
    )
  )
}

results_display_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(data_list$transformed())
      numeric_vars <- data_list$transformed() |> select(where(is.numeric)) |> names()
      req(length(numeric_vars) >= 2)
      updateSelectInput(session, "x_var", choices = numeric_vars, selected = numeric_vars[1])
      updateSelectInput(session, "y_var", choices = numeric_vars, selected = numeric_vars[2])
    })

    output$complete_summary <- renderPrint({
      req(data_list$transformed())
      summary(data_list$transformed())
    })

    output$correlation_matrix <- renderPlot({
      req(data_list$transformed())
      numeric_data <- data_list$transformed() |> select(where(is.numeric))
      req(ncol(numeric_data) > 1)
      corrplot::corrplot(cor(numeric_data, use = "complete.obs"), method = "color", type = "upper")
    })

    output$interactive_plot <- renderPlotly({
      req(data_list$transformed(), input$x_var, input$y_var)
      d <- data_list$transformed()
      p <- ggplot(d, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[input$color_var]])) +
        geom_point(alpha = .65) +
        theme_minimal() +
        labs(x = input$x_var, y = input$y_var, color = input$color_var)
      ggplotly(p)
    })

    output$correlation_test <- renderPrint({
      req(data_list$transformed(), input$x_var, input$y_var)
      d <- data_list$transformed()
      x <- d[[input$x_var]]
      y <- d[[input$y_var]]
      req(is.numeric(x), is.numeric(y))
      test <- cor.test(x, y, method = "pearson")
      cat("Pearson correlation\n")
      cat("r =", round(unname(test$estimate), 3), "\n")
      cat("p-value =", format.pval(test$p.value), "\n")
      cat("95% CI = [", round(test$conf.int[1], 3), ",", round(test$conf.int[2], 3), "]\n")
    })

    output$group_comparison <- renderPrint({
      req(data_list$transformed(), input$y_var, input$color_var)
      d <- data_list$transformed()
      req(is.numeric(d[[input$y_var]]))
      fit <- aov(reformulate(input$color_var, response = input$y_var), data = d)
      print(summary(fit))
    })

    output$filtered_data_table <- renderDT({
      req(data_list$filtered())
      datatable(data_list$filtered(), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$download_data <- downloadHandler(
      filename = function() paste0("haliotis_analysis_", Sys.Date(), ".csv"),
      content = function(file) write.csv(data_list$filtered(), file, row.names = FALSE)
    )
  })
}

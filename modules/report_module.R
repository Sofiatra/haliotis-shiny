report_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        4,
        h3("Analysis report"),
        wellPanel(
          textInput(ns("report_title"), "Report title:", value = "Haliotis Data Analysis"),
          textInput(ns("analyst_name"), "Analyst:", value = ""),
          selectInput(ns("report_format"), "Format:", choices = c("HTML" = "html", "PDF" = "pdf")),
          downloadButton(ns("generate_report"), "Generate report", class = "btn-success")
        )
      ),
      column(
        8,
        h4("Executive summary"),
        uiOutput(ns("executive_summary")),
        fluidRow(
          column(6, plotlyOutput(ns("report_plot1"))),
          column(6, plotlyOutput(ns("report_plot2")))
        )
      )
    )
  )
}

report_module_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    data_all <- reactive({
      req(data_list$transformed())
      data_list$transformed()
    })

    output$executive_summary <- renderUI({
      d <- data_all()
      tagList(
        p("This analysis contains ", strong(nrow(d)), " Haliotis observations."),
        p("Morphometric variables: ", strong("Length, Diameter, Height.")),
        p("Weight variables: ", strong("Whole.weight, Shucked.weight, Viscera.weight, Shell.weight.")),
        p("Age proxy: ", strong("Rings"), " with a derived age category.")
      )
    })

    output$report_plot1 <- renderPlotly({
      p <- ggplot(data_all(), aes(Sex, Rings, fill = Sex)) +
        geom_boxplot(alpha = .7) +
        theme_minimal() +
        labs(title = "Rings by sex", x = "Sex", y = "Rings")
      ggplotly(p)
    })

    output$report_plot2 <- renderPlotly({
      p <- ggplot(data_all(), aes(Length, Whole.weight, color = Age)) +
        geom_point(alpha = .7) +
        theme_minimal() +
        labs(title = "Length vs whole weight", x = "Length", y = "Whole weight")
      ggplotly(p)
    })

    output$generate_report <- downloadHandler(
      filename = function() {
        ext <- if (input$report_format == "html") "html" else "pdf"
        paste0("Haliotis_report_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        temp_report <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", temp_report, overwrite = TRUE)

        params <- list(
          report_title = input$report_title,
          analyst = input$analyst_name,
          data = data_all()
        )

        output_format <- if (input$report_format == "html") "html_document" else "pdf_document"

        rmarkdown::render(
          input = temp_report,
          output_format = output_format,
          output_file = basename(file),
          output_dir = dirname(file),
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      }
    )
  })
}

import_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        4,
        wellPanel(
          h3(icon("upload"), "Data import"),

          radioButtons(
            ns("app_language"),
            "Application language:",
            choices = c(
              "Français" = "fr",
              "English" = "en",
              "Italiano" = "it"
            ),
            selected = "en",
            inline = TRUE
          ),

          hr(),

          radioButtons(
            ns("file_type"),
            "File format:",
            choices = c(
              "TSV (tabulation)" = "\t",
              "CSV (comma)" = ",",
              "CSV (semicolon)" = ";",
              "Space-delimited" = " "
            ),
            selected = "\t"
          ),

          fileInput(
            ns("file_upload"),
            "Choose a file:",
            accept = c(".txt", ".tsv", ".csv"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),

          numericInput(
            ns("preview_rows"),
            "Rows to display:",
            value = 10,
            min = 5,
            max = 50,
            step = 5
          ),

          actionButton(
            ns("reset_data"),
            "Reset",
            icon = icon("refresh"),
            class = "btn-warning"
          )
        ),

        wellPanel(
          h4("Dataset information"),
          uiOutput(ns("data_info"))
        )
      ),

      column(
        8,
        h3("Data preview"),
        tabsetPanel(
          tabPanel("Table", DTOutput(ns("data_preview"))),
          tabPanel("Structure", verbatimTextOutput(ns("data_structure"))),
          tabPanel("Summary", verbatimTextOutput(ns("data_summary"))),
          tabPanel("Columns", DTOutput(ns("columns_info")))
        )
      )
    )
  )
}

import_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      language = "en",
      messages = list()
    )

    observe({
      rv$language <- input$app_language

      rv$messages <- switch(
        input$app_language,
        "fr" = list(
          no_file = "Aucun fichier importé",
          dimensions = "Dimensions",
          rows = "lignes",
          cols = "colonnes",
          variables = "variables",
          numeric = "numériques",
          categorical = "catégorielles",
          reset = "Données réinitialisées"
        ),
        "it" = list(
          no_file = "Nessun file importato",
          dimensions = "Dimensioni",
          rows = "righe",
          cols = "colonne",
          variables = "variabili",
          numeric = "numeriche",
          categorical = "categoriche",
          reset = "Dati reimpostati"
        ),
        list(
          no_file = "No file imported",
          dimensions = "Dimensions",
          rows = "rows",
          cols = "columns",
          variables = "variables",
          numeric = "numeric",
          categorical = "categorical",
          reset = "Data reset"
        )
      )
    })

    imported_data <- reactive({
      req(input$file_upload)

      tryCatch(
        readr::read_delim(
          input$file_upload$datapath,
          delim = input$file_type,
          show_col_types = FALSE,
          locale = readr::locale(encoding = "UTF-8")
        ),
        error = function(e) {
          showNotification(
            paste("Import error:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
    })

    create_default_data <- function() {
      set.seed(123)
      n <- 200

      tibble(
        Sex = sample(c("M", "F", "I"), n, replace = TRUE),
        Length = round(runif(n, 0.1, 0.8), 2),
        Diameter = round(runif(n, 0.07, 0.6), 2),
        Height = round(runif(n, 0.01, 0.3), 2),
        Whole.weight = round(runif(n, 0.1, 2.5), 3),
        Shucked.weight = round(runif(n, 0.05, 1.2), 3),
        Viscera.weight = round(runif(n, 0.01, 0.5), 3),
        Shell.weight = round(runif(n, 0.05, 1.0), 3),
        Rings = sample(1:30, n, replace = TRUE)
      )
    }

    final_data <- reactive({
      imported <- imported_data()
      if (!is.null(imported)) imported else create_default_data()
    })

    observeEvent(input$reset_data, {
      session$sendCustomMessage(
        type = "resetFileInput",
        message = session$ns("file_upload")
      )
      showNotification(rv$messages$reset, type = "message")
    })

    output$data_info <- renderUI({
      data <- final_data()
      msg <- rv$messages

      numeric_vars <- sum(vapply(data, is.numeric, logical(1)))
      categorical_vars <- sum(
        vapply(data, function(x) is.character(x) || is.factor(x), logical(1))
      )

      tagList(
        tags$p(
          strong(paste0(msg$dimensions, ": ")),
          nrow(data), msg$rows, "×", ncol(data), msg$cols
        ),
        tags$p(
          strong(paste0(msg$variables, ": ")),
          numeric_vars, msg$numeric, ",",
          categorical_vars, msg$categorical
        ),
        tags$hr(),
        tags$p(
          icon("database"),
          if (!is.null(imported_data())) "Imported file" else "Default synthetic dataset"
        )
      )
    })

    output$data_preview <- renderDT({
      datatable(
        head(final_data(), input$preview_rows),
        options = list(
          pageLength = input$preview_rows,
          scrollX = TRUE,
          dom = "t"
        )
      )
    })

    output$data_structure <- renderPrint({
      str(final_data())
    })

    output$data_summary <- renderPrint({
      summary(final_data())
    })

    output$columns_info <- renderDT({
      data <- final_data()

      col_info <- tibble(
        Column = names(data),
        Type = purrr::map_chr(data, ~ class(.x)[1]),
        Missing = purrr::map_int(data, ~ sum(is.na(.x))),
        Unique = purrr::map_int(data, ~ length(unique(.x)))
      )

      datatable(
        col_info,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    return(list(
      data = final_data,
      language = reactive(rv$language),
      messages = reactive(rv$messages)
    ))
  })
}

import_module_js <- function() {
  tags$script(HTML('
    Shiny.addCustomMessageHandler("resetFileInput", function(message) {
      const input = $("#" + message);
      if (input.length) {
        input.val("");
        input.trigger("change");
      }
    });
  '))
}

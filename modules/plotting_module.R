plotting_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        3,
        h3("Visualization controls"),
        selectInput(ns("sex_filter"), "Sex:", choices = c("All" = "all", "Immature" = "I", "Female" = "F", "Male" = "M")),
        sliderInput(ns("rings_filter"), "Rings:", min = 1, max = 30, value = c(1, 30)),
        selectInput(ns("age_filter"), "Age:", choices = c("All" = "all", "Young" = "young", "Adult" = "adult", "Old" = "old")),
        hr(),
        selectInput(ns("plot_type"), "Plot:", choices = c(
          "Rings vs sex" = "box_sex",
          "Viscera weight" = "stripchart",
          "Age distribution" = "age_dist",
          "Weight vs shell weight" = "scatter2d",
          "3D morphometric plot" = "scatter3d",
          "Correlation heatmap" = "corr_heat",
          "PCA 2D" = "pca2d",
          "PCA 3D" = "pca3d"
        )),
        checkboxInput(ns("show_correlation"), "Add regression line", TRUE),
        helpText("Filters are applied to the selected visualization.")
      ),
      column(
        9,
        h3(textOutput(ns("plot_title"))),
        plotlyOutput(ns("main_plot"), height = "550px"),
        hr(),
        h4("Statistical summary"),
        verbatimTextOutput(ns("plot_stats"))
      )
    )
  )
}

plotting_module_server <- function(id, raw_data, transformed_data) {
  moduleServer(id, function(input, output, session) {
    data_all <- reactive({ req(transformed_data()); transformed_data() })

    observe({
      d <- data_all()
      req(nrow(d) > 0, any(is.finite(d$Rings)))
      updateSliderInput(
        session, "rings_filter",
        min = min(d$Rings, na.rm = TRUE),
        max = max(d$Rings, na.rm = TRUE),
        value = range(d$Rings, na.rm = TRUE)
      )
    })

    filtered_data <- reactive({
      d <- data_all()
      if (input$sex_filter != "all") d <- d |> filter(Sex == input$sex_filter)
      d <- d |> filter(Rings >= input$rings_filter[1], Rings <= input$rings_filter[2])
      if (input$age_filter != "all") d <- d |> filter(Age == input$age_filter)
      d
    })

    output$plot_title <- renderText({
      c(
        box_sex = "Rings by sex",
        stripchart = "Viscera weight by sex",
        age_dist = "Age-category distribution",
        scatter2d = "Whole weight vs shell weight",
        scatter3d = "3D: length, whole weight and rings",
        corr_heat = "Correlation heatmap",
        pca2d = "PCA 2D",
        pca3d = "PCA 3D"
      )[[input$plot_type]]
    })

    output$main_plot <- renderPlotly({
      d <- filtered_data()
      req(nrow(d) > 1)

      if (input$plot_type == "box_sex") {
        p <- ggplot(d, aes(Sex, Rings, fill = Sex)) + geom_boxplot(alpha = .8) + theme_minimal()
        return(ggplotly(p))
      }

      if (input$plot_type == "stripchart") {
        p <- ggplot(d, aes(Sex, Viscera.weight, color = Sex)) + geom_jitter(width = .2, alpha = .7) + theme_minimal()
        return(ggplotly(p))
      }

      if (input$plot_type == "age_dist") {
        p <- ggplot(d, aes(Age, fill = Age)) + geom_bar() + theme_minimal()
        return(ggplotly(p))
      }

      if (input$plot_type == "scatter2d") {
        p <- ggplot(d, aes(Whole.weight, Shell.weight, color = Sex)) + geom_point(alpha = .7) + theme_minimal()
        if (isTRUE(input$show_correlation)) p <- p + geom_smooth(method = "lm", se = FALSE)
        return(ggplotly(p))
      }

      if (input$plot_type == "scatter3d") {
        return(plot_ly(d, x = ~Length, y = ~Whole.weight, z = ~Rings, color = ~Sex, type = "scatter3d", mode = "markers"))
      }

      if (input$plot_type == "corr_heat") {
        num <- d |> select(where(is.numeric))
        req(ncol(num) > 1)
        cm <- cor(num, use = "complete.obs")
        return(plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap"))
      }

      num <- d |> select(where(is.numeric)) |> drop_na()
      req(nrow(num) > 2, ncol(num) > 2)
      pca <- prcomp(num, scale. = TRUE)
      scores <- as.data.frame(pca$x)
      scores$Sex <- d$Sex[seq_len(nrow(scores))]

      if (input$plot_type == "pca2d") {
        return(plot_ly(scores, x = ~PC1, y = ~PC2, color = ~Sex, type = "scatter", mode = "markers"))
      }

      plot_ly(scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Sex, type = "scatter3d", mode = "markers")
    })

    output$plot_stats <- renderPrint({
      d <- filtered_data()
      req(nrow(d) > 1)
      if (input$plot_type == "scatter2d") {
        test <- cor.test(d$Whole.weight, d$Shell.weight)
        cat("Pearson correlation\n")
        cat("r =", round(unname(test$estimate), 3), "\n")
        cat("p-value =", format.pval(test$p.value), "\n")
      } else {
        print(summary(d))
      }
    })
  })
}

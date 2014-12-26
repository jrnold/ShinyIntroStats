library("shiny")
library("ggvis")
data("mtcars")

.data <- mtcars

shinyServer(function(input, output) {

  (.data
    %>% ggvis(~ wt)
    %>% layer_histograms(width = input_numeric(1, label = "Width"),
                         stack = FALSE)
    %>% bind_shiny("hist", "hist_ui")
  )

  output$table <- renderDataTable(
    .data
  )
  
  output$summary <- renderPrint(
    summary(.data)
  )
  
  output$stem <- renderPrint(
    stem(.data$wt)
  )
})

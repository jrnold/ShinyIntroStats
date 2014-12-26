library("shiny")
library("ggvis")
data("mtcars")

.data <- mtcars

  

shinyServer(function(input, output) {

  reactive({
    .data %>%
      ggvis(~ wt) %>%
      layer_histograms(width = input_numeric(1)) %>%
      bind_shiny("hist", "hist_ui")
  })
  
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

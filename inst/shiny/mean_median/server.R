library("shiny")
library("ggvis")

formatValues <- function(x) {
  if (is.na(x) || is.nan(x)) ""
  else prettyNum(x, format = "g", digits = 3)
}

shinyServer(function(input, output) {

  values <- reactiveValues()
  values$x <- numeric()

  new_entry <- observe({
    if(input$update > 0) {
      isolate(values$x <- c(values$x, input$newval))
    }
  })
  clear_entry <- observe({
    if(input$clear > 0) {
      isolate(values$x <- numeric())
    }
  })

  reactive({data_frame(x = values$x) %>%
             ggvis(~ x, singular()) %>%
             layer_points() %>%
             layer_points(x = mean(values$x), y = singular(), fill := "orange") %>%
             layer_points(x = median(values$x), y = singular(), fill := "purple")
  }) %>% bind_shiny("plot")

  output$median <- renderText(paste("Median:", formatValues(median(values$x))))
  output$mean <- renderText(paste("Mean:", formatValues(mean(values$x))))
  output$values <-
    renderText(paste("Values: ", paste(sapply(values$x, formatValues), collapse = ", ")))

})


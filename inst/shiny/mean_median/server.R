library("shiny")
library("ggvis")

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

  output$stats <-
    renderPrint(cat("Median:", median(values$x), "\nMean:", mean(values$x)))
  output$values <-
    renderText(paste("Values: ", paste(values$x, collapse = ", ")))

})


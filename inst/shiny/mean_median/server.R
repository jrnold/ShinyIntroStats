library("shiny")
library("ggplot2")

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

  output$plot <-
    (ggplot(values(), aes(x = x))
       + geom_dotplot()
       + geom_vline(xintercept = mean(values$x), colour = "purple")
       + geom_vline(xintercept = median(values$x), colour = "orange")
       + theme(axis.line = element_blank())
    )

  output$median <- renderText(paste("Median:", formatValues(median(values$x))))
  output$mean <- renderText(paste("Mean:", formatValues(mean(values$x))))
  output$values <-
    renderText(paste("Values: ", paste(sapply(values$x, formatValues), collapse = ", ")))

})


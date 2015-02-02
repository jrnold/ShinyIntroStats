library("shiny")
library("stringr")

shinyServer(function(input, output) {
  output$sample <-
    renderText(str_c(sample(input$min:input$max, size = input$n,
                            replace = input$replace),
                     collapse = ", "))
})

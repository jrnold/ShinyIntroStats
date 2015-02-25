library("shiny")
library("stringr")

shinyServer(function(input, output) {
  output$sample <-
    renderText({
      input$submit
      isolate(str_c(sample(input$min:input$max, size = input$n,
                            replace = input$replace),
                     collapse = ", "))
      })
})

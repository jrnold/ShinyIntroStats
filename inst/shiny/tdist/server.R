library("shiny")
library("ShinyIntroStats")

shinyServer(function(input, output) {
  limits <- c(-4, 4)
  max.sd <- 4

#   limitsq <- reactive({
#     if (input$lower_tail) {
#       c(-4, max(-4, input$q))
#     } else {
#       c(min(4, input$q), 4)
#     }
#   })

  limitst <- reactive({
    c(-4, max(-4, qt(input$p, input$df, lower.tail = TRUE)))
  })
  limitsnorm <- reactive({
    c(-4, max(-4, qnorm(input$p, lower.tail = TRUE)))
  })

  normtail <- reactive(qnorm(input$p, lower.tail = TRUE))
  ttail <- reactive(qt(input$p, df = input$df,
                       lower.tail = TRUE))

  output$plot <- renderPlot({
    (normal_plot(colour = "red", max.sd = max.sd)
     + geom_dt_line(df = input$df, limits[1], limits[2], colour = "blue")
     + geom_dnorm_area(xmin = limitsnorm()[1], xmax = limitsnorm()[2], fill = "red",
                       alpha = 0.3)
     + geom_dt_area(df = input$df, xmin = limitst()[1], xmax = limitst()[2],
                    fill = "blue", alpha = 0.3)
     + theme_minimal()
     + ggtitle(sprintf("degrees of freedom = %d", input$df)))
  })

  output$tailprob <- renderUI({
      withMathJax("Critical values for \\(P(X \\leq x)\\):")
  })

  output$qt <- renderText(sprintf("Student's t distribution: %f", ttail()))

  output$qnorm <- renderText(sprintf("Normal distribution: %f", normtail()))

})

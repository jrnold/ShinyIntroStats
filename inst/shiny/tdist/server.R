library("shiny")

shinyServer(function(input, output) {
  limits <- c(-4, 4)
  max.sd <- 4

  limitsq <- reactive({
    if (input$lower_tail) {
      c(-4, max(-4, input$q))
    } else {
      c(min(4, input$q), 4)
    }
  })

  normtail <- reactive(pnorm(input$q, lower.tail = input$lower_tail))
  ttail <- reactive(pt(input$q, df = input$df,
                       lower.tail = input$lower_tail))

  output$plot <- renderPlot({
    (normal_plot(colour = "red", max.sd = max.sd)
     + geom_dt_line(df = input$df, limits[1], limits[2], colour = "blue")
     + geom_dnorm_area(xmin = limitsq()[1], xmax = limitsq()[2], fill = "red",
                       alpha = 0.3)
     + geom_dt_area(df = input$df, xmin = limitsq()[1], xmax = limitsq()[2],
                    fill = "blue", alpha = 0.3)
     + theme_minimal()
     + ggtitle(sprintf("degrees of freedom = %d", input$df)))
  })

  output$tailprob <- renderUI({
    if (input$lower_tail) {
      withMathJax("Tail Probabilities \\(P(X \\leq x)\\):")
    } else {
      withMathJax("Tail Probabilities \\(P(X > x)\\):")
    }
  })

  output$pt <- renderText(sprintf("Student's t distribution: %f", ttail()))

  output$pnorm <- renderText(sprintf("Normal distribution: %f", normtail()))

})

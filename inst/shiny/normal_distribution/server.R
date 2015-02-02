library("ggplot2")

shinyServer(function(input, output) {
  output$plot <-
      renderPlot({
          n <- 1000
          x <- seq(input$mean1 - 6 * input$sd2,
                   input$mean2 + 6 * input$sd2,
                   length.out = n)
          y1 <- dnorm(x, input$mean1, input$sd1)
          y2 <- dnorm(x, input$mean2, input$sd2)
          dd <- data.frame(x = rep(x, 2),
                           y = c(y1, y2),
                           distribution = factor(rep(c(1L, 2L), each = n)))
          breaks <- input$mean1 + -6:6 * input$sd2
          (ggplot(dd, aes(x = x, ymax = y, ymin = 0, fill = distribution))
           + geom_ribbon(alpha = 0.5)
           + scale_y_continuous("p(x)")
           + scale_x_continuous("x", breaks = breaks, limits = range(breaks)))
      })
})


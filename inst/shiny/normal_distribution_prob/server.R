library("ggplot2")
library("xtable")

normal_prob_area_plot <- function(q, mean = 0, sd = 1, lower.tail = TRUE, n = 1000) {
  max.sd <- 4
  limits <- mean + max.sd * c(-1, 1) * sd
  x <- seq(limits[1], limits[2], length.out = n)
  if (lower.tail) {
    xmin <- limits[1]
    xmax <- q
  } else {
    xmin <- q
    xmax <- limits[2]
  }
  areax <- seq(xmin, xmax, length.out = n)
  area <- data.frame(x = areax, ymin = 0, ymax = dnorm(areax, mean = mean, sd = sd))
  breaks <- -max.sd:max.sd * sd

  (ggplot()
   + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax))
   + scale_x_continuous("x", limits = limits,
                        breaks = breaks)
   + scale_y_continuous("P(x)"))
}

shinyServer(function(input, output) {
  q <- reactive({
    if (input$inputType == "q") input$q
    else qnorm(input$p, input$mean, input$sd, lower.tail = input$lower.tail)
  })
  p <- reactive({
    if (input$inputType == "p") input$p
    else pnorm(input$q, input$mean, input$sd, lower.tail = input$lower.tail)
  })

  output$plot <-
    renderPlot(normal_prob_area_plot(q(), mean = input$mean,
                                     sd = input$sd,
                                     lower.tail = input$lower.tail))
  output$table <- renderUI({
    x0 <- c("Quantile", "\\(z\\)-score", "Probability")
    x1 <- c("$$x$$",
            "$$z = \\frac{(x - \\mu)}{\\sigma}$$",
            if (input$lower.tail) "$$P(X \\leq x)$$" else "$$P(X > x)$$")
    x2 <- c(prettyNum(q(), digits = 3, format = "f"),
            prettyNum((q() - input$mean) / input$sd,
                      digits = 3, format = "f"),
            prettyNum(p(), digits = 3, format = "f"))
    withMathJax(HTML(print(xtable(cbind(x0, x1, x2)),
                     include.colnames = FALSE, include.rownames = FALSE,
                     type = "html")))
  })
})


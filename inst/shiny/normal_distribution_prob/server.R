library("ggplot2")
library("xtable")
library("ShinyIntroStats")


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
    renderPlot({
      normal_tail_plot_q(q(), mean = input$mean, sd = input$sd,
                                  lower.tail = input$lower.tail) +
      theme_minimal()
    })
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


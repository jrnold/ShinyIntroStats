library("stringr")
source("utils.R")
shinyServer(function(input, output) {
  se <- reactive((input$sigma / sqrt(input$n)))
  z <- reactive({
    (input$xbar - input$mu) / se()
  })

  pval <- reactive({
    if (input$direction == "lt") {
      p <- pnorm(z())
    } else if (input$direction == "gt") {
      p <- pnorm(z(), lower.tail = FALSE)
    } else {
      p <- 2 * pnorm(abs(z()))
    }
  })

  output$plot <- renderPlot({
    normal_tail_plot_q(input$xbar,
                     mean = input$mu,
                     sd = se(),
                     lower.tail = (input$direction == "lt"),
                     two.sided = (input$direction == "neq")) +
      scale_x_continuous("",
                         breaks = c(input$mu, input$xbar, -input$xbar),
                         labels = c(expression(mu),
                                    expression(bar(x)),
                                    expression(-bar(x)))) +
      theme_minimal()
  })

  output$H0 <- renderUI({
    withMathJax(str_c("\\(H_0\\): \\( \\mu = ", input$mu, "\\)"))
  })

  output$Ha <- renderUI({
    withMathJax(str_c("\\(H_a\\): \\( \\mu ",
                      switch(input$direction,
                      lt = "<", gt = ">", neq = "\\neq"),
                      input$mu, "\\)"))
  })

  output$pval <- renderText({
    sprintf("p-value: %4g", round(pval(), 4))
  })

  output$z <- renderText({
    sprintf("test statistic: %g", z())
  })


})


library("stringr")
library("ggplot2")
source("utils.R")

shinyServer(function(input, output) {
  se <- reactive((input$sigma / sqrt(input$n)))
  z <- reactive({
    (input$xbar - input$mu) / se()
  })

  pval <- reactive({
    if (input$use_normal) {
      if (input$direction == "lt") {
        p <- pnorm(z())
      } else if (input$direction == "gt") {
        p <- pnorm(z(), lower.tail = FALSE)
      } else {
        p <- 2 * pnorm(-abs(z()))
      }
    } else {
      if (input$direction == "lt") {
        p <- pt(z(), input$n - 1)
      } else if (input$direction == "gt") {
        p <- pt(z(), input$n - 1, lower.tail = FALSE)
      } else {
        p <- 2 * pt(-abs(z()), input$n - 1)
      }
    }

  })

  output$plot <- renderPlot({
    if (input$use_normal) {
      plt <- normal_tail_plot_q(z(),
                                lower.tail = (input$direction == "lt"),
                                two.sided = (input$direction == "neq"),
                                area_opts = list(alpha = 0.5))
    } else {
      plt <- students_t_tail_plot_q(z(),
                                    df = input$n - 1,
                                    lower.tail = (input$direction == "lt"),
                                    two.sided = (input$direction == "neq"),
                                    area_opts = list(alpha = 0.5))
    }
    plt +
      scale_x_continuous(expression((bar(x) - mu[0]) / (s / sqrt(n))),
                         breaks = unique(c(0, -z(), z()))) +
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

  output$z <- renderUI({
    withMathJax(sprintf("test statistic (\\(%s\\)): %g",
                        if (input$use_normal) "z" else "t",
                        z()))
  })


})


library("ggplot2")
library("dplyr")

norm_mean <- 0
norm_sd <- 1

##' Take sample from normal dist and calculate confidence interval for normal
draw_ci_ <- function(n, conf_level = 0.95, mu=0, sigma=1) {
    smpl <- rnorm(n, mu, sigma)
    smpl_sd <- sd(smpl)
    smpl_mean <- mean(smpl)
    tailprob <- (1 - conf_level) / 2
    q <- -qt(tailprob, df=(n - 1), lower.tail=TRUE)
    se <- smpl_sd / sqrt(n)
    data_frame(lb = smpl_mean - q * se,
               ub = smpl_mean + q * se,
               mean = smpl_mean,
               se = se,
               n = n,
               contains_mean = ((mu > lb) & (mu < ub)))
}

draw_ci <- function(nsamples, n, conf_level = 0.95, mu=0, sigma=1) {
   data_frame(i = seq_len(nsamples)) %>%
     group_by(i) %>%
     do(draw_ci_(n, conf_level, mu, sigma)) %>%
     ungroup()
}

shinyServer(function(input, output) {
    sample_ci <- reactive({
      input$draw
      isolate({
        x <- draw_ci(input$samples, input$n, input$confidence / 100,
                     input$mu, input$sigma)
        if (input$sorted) {
          arrange(x, mean) %>% mutate(i = seq_along(lb))
        } else x
      })
    })

    output$plot <- renderPlot({
        (ggplot(sample_ci(), aes(x = i,
                                 ymin = lb, ymax = ub,
                                 colour = contains_mean))
              + geom_hline(xintercept = 0, colour="blue")
              + geom_linerange()
              + coord_flip()
              + scale_x_continuous("")
              + scale_y_continuous(sprintf("%d%% CI", input$confidence))
              + scale_colour_manual(values = c("black", "gray"))
              + theme_minimal()
              + theme(legend.position = "none",
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())
        )
    })

    output$npct <- renderText({
        c("Percent of CI containing the population mean:",
          round(mean(sample_ci()$contains_mean) * 100, 2), "%")
    })
})

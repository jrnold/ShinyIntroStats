library("ggplot2")
library("dplyr")

rbernoulli <- function(n, p) sample(0:1, n, replace = TRUE,
                                     prob = c(1 - p, p))

##' Take sample from normal dist and calculate confidence interval for normal
draw_ci_ <- function(n, p = 0.5, conf_level = 0.95, plus_four = FALSE) {
    smpl <- rbernoulli(n, p)
    if (plus_four) {
      smpl_p <- mean(c(smpl, c(0, 0, 1, 1)))
    } else {
      smpl_p <- mean(smpl)
    }
    tailprob <- (1 - conf_level) / 2
    q <- -qnorm(tailprob, lower.tail=TRUE)
    se <- sqrt( smpl_p * (1 - smpl_p) / n)
    data_frame(lb = smpl_p - q * se,
               ub = smpl_p + q * se,
               phat = smpl_p,
               se = se,
               n = n,
               contains_p = ((p > lb) & (p < ub)))
}

draw_ci <- function(nsamples, n, p = 0.5, conf_level = 0.95,
                    plus_four = FALSE) {
   data_frame(i = seq_len(nsamples)) %>%
     group_by(i) %>%
     do(draw_ci_(n, p, conf_level, plus_four)) %>%
     ungroup()
}

shinyServer(function(input, output) {
    sample_ci <- reactive({
      input$draw
      isolate({
        x <- draw_ci(input$samples,
                     input$n,
                     input$p,
                     input$confidence / 100,
                     input$plus_four)
        if (input$sorted) {
          arrange(x, phat) %>% mutate(i = seq_along(phat))
        } else x
      })
    })

    output$plot <- renderPlot({
       input$draw
       isolate({
         (ggplot(sample_ci(), aes(x = i,
                                  ymin = lb, ymax = ub,
                                  colour = contains_p))
          + geom_linerange()
          + geom_hline(yintercept = input$p, colour="blue")
          + coord_flip()
          + scale_x_continuous("")
          + scale_y_continuous(sprintf("%d%% CI", input$confidence))
          + scale_colour_manual(values = c("FALSE"="black", "TRUE"="gray"))
          + theme_minimal()
          + theme(legend.position = "none",
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
         )
       })
    })

    output$npct <- renderText({
        c("Percent of samples with confidence intervals containing the population proportion:",
          round(mean(sample_ci()$contains_p) * 100, 2), "%")
    })
})

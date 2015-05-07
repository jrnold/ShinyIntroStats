library("ggplot2")
library("dplyr")

norm_mean <- 0
norm_sd <- 1

##' Take sample from normal dist and calculate confidence interval for normal
draw_ci_ <- function(n, conf_level = 0.95, mu=0, sigma=1,
                     use_normal = FALSE,
                     known_sd = FALSE) {
    smpl <- rnorm(n, mean = mu, sd = sigma)
    print(summary(smpl))
    smpl_sd <- sd(smpl)
    smpl_mean <- mean(smpl)
    tailprob <- (1 - conf_level) / 2
    if (use_normal) {
      q <- - qnorm(tailprob, lower.tail=TRUE)
    } else {
      q <- - qt(tailprob, df=(n - 1), lower.tail=TRUE)
    }
    if (known_sd) {
      se <- sigma / sqrt(n)
    } else {
      se <- smpl_sd / sqrt(n)
    }
    data_frame(lb = smpl_mean - q * se,
               ub = smpl_mean + q * se,
               mean = smpl_mean,
               se = se,
               n = n,
               contains_mean = ((mu > lb) & (mu < ub)))
}

draw_ci <- function(nsamples, n, conf_level = 0.95, mu=0, sigma=1,
                    use_normal = FALSE, known_sd = FALSE) {
   data_frame(i = seq_len(nsamples)) %>%
     group_by(i) %>%
     do(draw_ci_(n, conf_level,
                 mu = mu, sigma = sigma,
                 use_normal = use_normal,
                 known_sd = known_sd)) %>%
     ungroup()
}

shinyServer(function(input, output) {
    sample_ci <- reactive({
      input$draw
      isolate({
        x <- draw_ci(input$samples, input$n, input$confidence / 100,
                     mu = input$mu, sigma = input$sigma,
                     use_normal = input$use_normal,
                     known_sd = input$known_sd)
        if (input$sorted) {
          arrange(x, mean) %>% mutate(i = seq_along(mean))
        } else x
      })
    })

    mu <- reactive(input$mu)

    output$plot <- renderPlot({
       input$draw

       isolate({
         (ggplot(sample_ci(), aes(x = i,
                                  ymin = lb, ymax = ub,
                                  colour = contains_mean))
          + geom_linerange()
          + geom_hline(yintercept = mu(), colour="blue")
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

    output$eqn <- renderUI({
      input$draw
      isolate({
        n <- input$n
        conf_level <- input$confidence / 100
        tailprob <- (1 - conf_level) / 2
        if (input$use_normal) {
          score <- - qnorm(tailprob, lower.tail=TRUE)
        } else {
          score <- - qt(tailprob, df=(n - 1), lower.tail=TRUE)
        }
        withMathJax(sprintf(paste0("Confidence Intervals calcualted as ",
                                   "$$\\bar{x} \\pm %s \\frac{%s}{\\sqrt{%s}}$$"),
                    round(score, 2),
                    if (input$known_sd) round(input$sigma) else "s",
                    input$n))
        withMathJax(sprintf("Confidence Intervals calcualted as $$\\bar{x} \\pm %s \\cdot \\frac{%s}{\\sqrt{%s}}$$",
                            round(score, 2), if (input$known_sd) input$sigma else "s", input$n))
      })
    })

    output$npct <- renderText({
        c("Percent of samples with confidence intervals containing the population mean:",
          round(mean(sample_ci()$contains_mean) * 100, 2), "%")
    })
})

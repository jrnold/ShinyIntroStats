library("dplyr")
library("ggplot2")
library("stringr")
#library("parallel")
#library("ggvis")

shinyServer(function(input, output) {

  pfunc <- reactive({
    if (input$distr == "norm") {
      function(q) pnorm(q, mean = input$norm_mean, sd = input$norm_sd)
    } else if (input$distr == "bernoulli") {
      function(q) pbinom(q, 1, prob = input$bernoulli_prob)
    } else if (input$distr == "beta") {
      function(q) pbeta(q, input$beta_shape1, input$beta_shape2)
    } else if (input$distr == "exp") {
      function(q) pexp(q, input$exp_rate)
    } else if (input$distr == "gamma") {
      function(q) pgamma(q, input$gamma_shape, input$gamma_scale)
    } else if (input$distr == "unif") {
      function(q) punif(q, input$unif_min, input$unif_max)
    }
  })

  qfunc <- reactive({
    if (input$distr == "norm") {
      function(p) qnorm(p, mean = input$norm_mean, sd = input$norm_sd)
    } else if (input$distr == "bernoulli") {
      function(p) qbinom(p, 1, prob = input$bernoulli_prob)
    } else if (input$distr == "beta") {
      function(p) qbeta(p, input$beta_shape1, input$beta_shape2)
    } else if (input$distr == "exp") {
      function(p) qexp(p, input$exp_rate)
    } else if (input$distr == "gamma") {
      function(p) qgamma(p, input$gamma_shape, input$gamma_scale)
    } else if (input$distr == "unif") {
      function(p) qunif(p, input$unif_min, input$unif_max)
    }
  })

  rfunc <- reactive({
    if (input$distr == "norm") {
      function(n) rnorm(n, mean = input$norm_mean, sd = input$norm_sd)
    } else if (input$distr == "bernoulli") {
      function(n) rbinom(n, 1, prob = input$bernoulli_prob)
    } else if (input$distr == "beta") {
      function(n) rbeta(n, input$beta_shape1, input$beta_shape2)
    } else if (input$distr == "exp") {
      function(n) rexp(n, input$exp_rate)
    } else if (input$distr == "gamma") {
      function(n) rgamma(n, input$gamma_shape, input$gamma_scale)
    } else if (input$distr == "unif") {
      function(n) runif(n, input$unif_min, input$unif_max)
    }
  })

  mu <- reactive({
    if (input$distr == "norm") {
      mu <- input$norm_mean
    } else if (input$distr == "bernoulli") {
      mu <- input$bernoulli_prob
    } else if (input$distr == "beta") {
      a <- input$beta_shape1
      b <- input$beta_shape2
      mu <- a / (a + b)
    } else if (input$distr == "exp") {
      mu <- 1 / as.numeric(input$exp_rate)
    } else if (input$distr == "gamma") {
      mu <- as.numeric(input$gamma_shape) * as.numeric(input$gamma_scale)
    } else if (input$distr == "unif") {
      mu <- 0.5 * (input$unif_max - input$unif_min)
    }
    mu
  })

  sigma <- reactive({
    if (input$distr == "norm") {
      sigma <- input$norm_sd
    } else if (input$distr == "bernoulli") {
      sigma <- sqrt(input$bernoulli_prob * (1 - input$bernoulli_prob))
    } else if (input$distr == "beta") {
      a <- as.numeric(input$shape1)
      b <- as.numeric(input$shape2)
      sigma <- sqrt((a * b) / ((a + b)^2 * (a + b + 1)))
    } else if (input$distr == "exp") {
      sigma <- 1 / as.numeric(input$exp_rate)
    } else if (input$distr == "gamma") {
      sigma <- sqrt(as.numeric(input$gamma_shape) * as.numeric(input$gamma_scale)^2)
    } else if (input$distr == "unif") {
      sigma <- sqrt(1/12 * (input$unif_max - input$unif_min) ^ 2)
    }
    sigma
  })

  sample_size <- reactive({2 ^ input$obs})

  sampledist <- reactive({
    input$action
    samples <- 2^12
    sampler <- rfunc()
    n <- sample_size()
    f <- function(i) mean(sampler(n))
    sapply(seq_len(samples), f)
  })

  sample_mean <- reactive(mean(sampledist()))

  sample_sd <- reactive(sd(sampledist()))

  # For plot limits use the 1st and 99th percentiles of the population
  xlimits <- reactive({
    if (input$distr %in% c("beta")) {
      c(0, 1)
    } else if (input$distr %in% c("bernoulli")) {
      c(0, 1.1)
    } else if (input$distr %in% c("exp", "gamma")) {
      c(0, qfunc()(0.99))
    } else {
      c(qfunc()(0.01), qfunc()(0.99))
    }
  })

  output$table <- renderTable({
    x <-  data.frame(pop = c(mu(), sigma()),
                     sampling = c(sample_mean(), c(sample_sd())))
    rownames(x) <- c("Mean", "Std. Dev.")
    colnames(x) <- c("Population", "Sampling Dist.")
    x
  })

  ## Plot
  output$plot <- renderPlot({

    x <- sampledist()
    n <- length(x)
    r <- diff(range(x))
    binwidth <- r / (log2(n) + 1)

    #     # Printing the plot (needed for ggplot2 object)
    #     print(p)
    #     data_frame(x = x) %>%
    #         ggvis(~ x) %>%
    #         layer_histograms() %>%
    #         bind_shiny("ggvis", "ggvis_ui")

    p <- (ggplot(data=data_frame(x = sampledist()),
                 aes(x = x, y= ..density.. ))
          + ggtitle(paste("n =", 2^input$obs))
          + xlab("Sample Means")
          + coord_cartesian(xlim=xlimits())
          + geom_histogram(binwidth = binwidth, fill = "gray")
          + theme_minimal()
    )
    if (input$draw_normal) {
      normdata <- data_frame(x = seq(xlimits()[1], xlimits()[2], length.out = 2^10),
                             y = dnorm(x, mean = sample_mean(), sd = sample_sd()))
      p <- p + geom_line(data = normdata, mapping = aes(x = x, y = y),
                         colour = "black")
    }
    p
  })

#   reactive({
#     data_frame(x = sampledist()) %>%
#       ggvis(~ x) %>%
#       layer_histograms() %>%
#       bind_shiny("ggvis", "ggvis_ui")
#   })

})

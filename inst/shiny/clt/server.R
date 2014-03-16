## Copyright: Martin Berlin, Jeffrey Arnold <jeffrey.arnold@gmail.com> (2013)
##
## Adapted from original code by Martin Berlin <mno.berlin@gmail.com> http://spark.rstudio.com/berlin/stat/

library(shiny)
library(plyr)
library(ggplot2)

## Generate data

rbernoulli <- function(n, p) {
    sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
}

shinyServer(function(input, output) {

    data <- reactive({
        statistic <- mean
        ## statistic <- switch(input$statistic,
        ##                     mean = mean,
        ##                     median = median)
        obs <- 2^input$obs
        FUN <- switch(input$distribution,
                      normal = function() rnorm(obs, input$mean, input$sd),
                      bernoulli = function() rbernoulli(obs, input$p),
                      beta = function() rbeta(obs, as.numeric(input$shape1), as.numeric(input$shape2)),
                      gamma = function() rgamma(obs, as.numeric(input$shape), scale = as.numeric(input$scale)),
                      unif = function() runif(obs))
        data.frame(x1 = raply(input$draws, statistic(FUN())))
    })

    output$sample_mean <- renderText({
        paste("Sampling Dist Mean:", round(mean(data()$x1), 3))
    })
    output$sample_sd <- renderText({
        paste("Standard Error:", round(sd(data()$x1), 3))
    })
    output$pop_mean <- renderText({
        if (input$distribution == "normal") {
            mu <- input$mean
        } else if (input$distribution == "bernoulli") {
            mu <- input$p
        } else if (input$distribution == "beta") {
            mu <- as.numeric(input$shape1) / (as.numeric(input$shape1) + as.numeric(input$shape2))
        } else if (input$distribution == "gamma") {
            mu <- as.numeric(input$shape) * as.numeric(input$scale)
        } else if (input$distribution == "unif") {
            mu <- 0.5
        }
        paste("Population Mean:", round(mu, 3))
    })
    output$pop_sd <- renderText({
        if (input$distribution == "normal") {
            sigma <- input$sd
        } else if (input$distribution == "bernoulli") {
            sigma <- sqrt(input$p * (1 - input$p))
        } else if (input$distribution == "beta") {
            sigma <- sqrt(input$p * (1 - input$p))
        } else if (input$distribution == "beta") {
            a <- as.numeric(input$shape1)
            b <- as.numeric(input$shape2)
            sigma <- sqrt((a * b) / ((a + b)^2 * (a + b + 1)))
        } else if (input$distribution == "gamma") {
            sigma <- sqrt(as.numeric(input$shape) * as.numeric(input$scale)^2)
        } else if (input$distribution == "unif") {
            sigma <- sqrt(1/12)
        }
        paste("Population Std. Dev.:", round(sigma, 3))
    })

    
    ## Plot
    output$simPlot <- renderPlot({

        p  <- (ggplot(data=data(), aes(x=x1, y=..density..))
               + ggtitle(paste("n =", 2^input$obs))
               #+ xlab(input$statistic)
               + xlab("Mean")
               + coord_cartesian(xlim=input$xlim, ylim=input$ylim))
        
        # Histogram layer
        
        if (input$histogram==TRUE) {
            p <- p + geom_histogram(binwidth=input$bw)
        }
        
        # Kernel denity layer
        
        if (input$density==TRUE) {
            p <- p + geom_density(col="red")
        }
        
        # Printing the plot (needed for ggplot2 object)
        print(p)
    })
})

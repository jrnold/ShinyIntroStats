## Copyright: Martin Berlin, Jeffrey Arnold <jeffrey.arnold@gmail.com> (2013)
##
## Adapted from original code by Martin Berlin <mno.berlin@gmail.com> http://spark.rstudio.com/berlin/stat/
## https://gist.github.com/martin-berlin/fbfa870e72a3da4d67ee#file-stat-ui-r
library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Sampling Distribution of Sample Means"),

    sidebarPanel(
        selectInput("distribution", "Distribution:",
                    list("Normal" = "normal",
                         "Bernoulli" = "bernoulli",
                         "Uniform" = "unif",
                         "Beta" = "beta",
                         "Gamma" = "gamma")),

        conditionalPanel(
            condition = "input.distribution == 'normal'",
            sliderInput("mean", "mean: ", min=0, max=400, value=0),
            sliderInput("sd", "st. dev.: ", min=0.1, max=20, value=1, step=.1)
            ),
        conditionalPanel(
            condition = "input.distribution == 'bernoulli'",
            sliderInput("p", "proportion: ", min=0.0, max=1, value=0.5, step=0.1)
            ),
        conditionalPanel(
            condition = "input.distribution == 'beta'",
            selectInput("shape1", "shape1: ", c(0.5, 1, 2, 3, 5)),
            selectInput("shape2", "shape2: ", c(0.5, 1, 2, 3, 5))
            ),
        conditionalPanel(
            condition = "input.distribution == 'gamma'",
            selectInput("shape", "shape: ", c(1, 2, 3, 5, 9)),
            selectInput("scale", "scale: ", c(0.5, 1, 2))
            ),
        
        ## radioButtons("statistic", "Statistic:",
        ##              list("Mean" = "mean",
        ##                   "Median" = "median")),

        br(),

        sliderInput("obs", "Sample Size:", 0, 11, 0, step = 1,
                    ticks = 2^(0:11),
                    animate=animationOptions(interval=1000, loop=F)),
        helpText("Note: Press 'play' button to start animation."),
        
        sliderInput("draws", "Draws:", 
                    min=10, max=1000, value=500),

        br(),

        checkboxInput("histogram", "Histogram", TRUE),

        conditionalPanel(condition= "input.histogram==true",
                         sliderInput("bw", "Bin width:", min=0.01, max=0.2, value=0.05,
                                     step=0.01)),

        checkboxInput("density", "Density", FALSE),

        conditionalPanel(condition = "input.density == true"),

        checkboxInput("plotdefault", "Show plot settings", FALSE),

        conditionalPanel(condition= "input.plotdefault==true",
                         sliderInput("xlim", "x-range:", min=-3, max=4, value=c(-2,2), 
                                     step=0.5),
                         sliderInput("ylim", "y-range:", min=0, max=50, value=c(0,5),
                                     step=0.5))
        ),
    
    mainPanel(
        textOutput("pop_mean"),
        textOutput("pop_sd"),
        textOutput("sample_mean"),
        textOutput("sample_sd"),
        plotOutput("simPlot")
        )))


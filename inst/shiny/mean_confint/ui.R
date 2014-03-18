library("shiny")
 
shinyUI(pageWithSidebar(
    headerPanel("Confidence Intervals of the Mean"),

    sidebarPanel(
        p(strong("Confidence Interval 1")),
        sliderInput("xbar1", "Sample mean:", 0, min = -5, max = 5,
                     step=0.1),
        sliderInput("s1", "Sample standard deviation:", 1, min = 0.1, max = 5,
                    step=0.1),
        sliderInput("conf1", "Confidence (%):", 95, min = 1, max = 99,
                     step=1),
        sliderInput("n1", "Sample size:", 100, min = 1, max = 10000,
                    step=1),
        p(strong("Confidence Interval 2")),
        sliderInput("xbar2", "Sample mean:", 0, min = -5, max = 5,
                     step=0.1),
        sliderInput("s2", "Sample standard deviation:", 1, min = 0.1, max = 5,
                    step=0.1),
        sliderInput("conf2", "Confidence (%):", 95, min = 1, max = 99,
                     step=1),
        sliderInput("n2", "Sample size:", 100, min = 1, max = 10000,
                     step=1)
        ),

    mainPanel(
        plotOutput("plot")
    )))

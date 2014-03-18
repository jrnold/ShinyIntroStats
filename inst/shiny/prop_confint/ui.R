library("shiny")
 
shinyUI(pageWithSidebar(
    headerPanel("Confidence Interval of the Proportion"),

    sidebarPanel(
        p(strong("Confidence Interval 1")),
        sliderInput("p1", "Sample proportion:", 0.5, min = 0.01, max = 0.99,
                     step=0.01),
        sliderInput("conf1", "Confidence (%):", 95, min = 1, max = 99,
                     step=1),
        sliderInput("n1", "Sample size:", 100, min = 1, max = 10000,
                    step=1),
        p(strong("Confidence Interval 2")),
        sliderInput("p2", "Sample proportion:", 0.5, min = 0.01, max = 0.99,
                     step=0.01),
        sliderInput("conf2", "Confidence (%):", 95, min = 1, max = 99,
                     step=1),
        sliderInput("n2", "Sample size:", 100, min = 1, max = 10000,
                     step=1)
        ),

    mainPanel(
        plotOutput("plot")
    )))

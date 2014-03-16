library("shiny")
 
shinyUI(pageWithSidebar(
    headerPanel("Confidence Intervals"),

    sidebarPanel(
        actionButton("draw", "Draw samples"),
        numericInput("confidence", "Confidence (%):", 95, min = 1, max = 99,
                     step=1),
        numericInput("smplsize", "Sample sizes:", 100, min = 2, max = 1000,
                     step=1),
        numericInput("nsamples", "Number of confidence intervals:", 100, min = 1, max = 500,
                     step=1)
        ),

    mainPanel(
        textOutput("npct"),
        plotOutput("plot"),
        p("The population has a mean of 0 and a standard deviation of 1.",
          "Intervals including the population mean are colored red, those not including the population mean are colored black")
    )))

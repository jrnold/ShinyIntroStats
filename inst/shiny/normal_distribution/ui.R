shinyUI(fluidPage(
  titlePanel("Comparing Normal Distributions"),
  sidebarLayout(
    sidebarPanel(p("Compare two normal distributions"),
                 strong("Distribution 1"),
                 numericInput("mean1", label = "Mean",
                              value = 0, step = 0.01),
                 numericInput("sd1", label = "Std. Dev.",
                              value = 1, step = 0.01, min = 0),
                 br(),
                 strong("Distribution 2"),
                 numericInput("mean2", label = "Mean",
                              value = 0, step = 0.01),
                 numericInput("sd2", label = "Std. Dev.",
                              value = 1, step = 0.01, min = 0),
                 helpText("The plot shows values on the scale of the 2nd distribution.")
                 ),
    mainPanel("", plotOutput("plot"))
  )
))

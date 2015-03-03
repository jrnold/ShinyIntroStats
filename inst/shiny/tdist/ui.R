library("shiny")
library("ggplot2")

shinyUI(
  fluidPage(
    titlePanel("Student's \\(t\\) distribution"),
    sidebarLayout(
      sidebarPanel(
        numericInput("df", "degrees of freedom", 7),
        h4("Tail probabilities"),
        numericInput("p", "Probability", 0.025, min = 0, max = 1, step = 0.01),
        # checkboxInput("lower_tail", "Lower tail?", TRUE),
        withMathJax()
      ),
      mainPanel(
        plotOutput("plot"),
        helpText("Red line is the normal distribution; the blue line is a Student's \\(t\\) distribution."),
        uiOutput("tailprob"),
        textOutput("qt"),
        textOutput("qnorm")
      )
    )
  )
)

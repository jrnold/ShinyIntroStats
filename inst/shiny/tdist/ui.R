shinyUI(
  fluidPage(
    titlePanel("Student's \\(t\\) distribution"),
    sidebarLayout(
      sidebarPanel(
        numericInput("df", "degrees of freedom", 1),
        h4("Tail probabilities"),
        numericInput("q", "Quantile value", -2),
        checkboxInput("lower_tail", "Lower tail?", TRUE),
        withMathJax()
      ),
      mainPanel(
        plotOutput("plot"),
        helpText("Red line is the normal distribution; the blue line is a Student's \\(t\\) distribution."),
        uiOutput("tailprob"),
        textOutput("pt"),
        textOutput("pnorm")
      )
    )
  )
)

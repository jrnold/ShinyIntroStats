shinyUI(
  fluidPage(
    titlePanel("Confidence Intervals of the Sample Mean"),

    #withMathJax(),
    sidebarLayout(

      sidebarPanel(
        actionButton("draw", "Draw samples"),
        numericInput("confidence", "Confidence (%):", 95, min = 1, max=99, step=1),
        numericInput("n", "Sample sizes:", 30, min = 1, step=1),
        numericInput("samples", "Number of confidence intervals:", 100, min = 1,
                     step=1),
        checkboxInput("sorted", "Sort confidence intervals by \\(\\bar{x}\\)",
                      FALSE),
        helpText("Population distributed normal with the following mean and standard deviation."),
        numericInput("mu", "mean (\\(\\mu\\))", 0),
        numericInput("sigma", "standard deviation (\\(\\sigma\\))", 1,
                     min = 0),
        checkboxInput("known_sd", "Use population \\(\\sigma\\)?", FALSE),
        helpText("Use the population standard deviation \\(\\sigma\\) to ",
                 "calculate the margin of error in the confidence intervals. ",
                 "By default the sample standard deviations are used."),
        checkboxInput("use_normal", "Use normal distribution?", FALSE),
        helpText("Use the normal distribution \\(z\\)-scores to calculate the ",
                 "margin of error in the confidence intervals. By default it ",
                 "uses the Student's \\(t\\) distribution.")
      ),

      mainPanel(
        plotOutput("plot"),
        textOutput("npct"),
        p("Blue line is the population mean \\(\\mu\\). ",
          "Black intervals do not contain\\(\\mu\\)."),
        uiOutput("eqn"),
        withMathJax()
      )
    )
  )
)

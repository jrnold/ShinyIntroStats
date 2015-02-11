shinyUI(
  fluidPage(
    titlePanel("Confidence Intervals of the Sample Mean"),

    #withMathJax(),
    sidebarLayout(

      sidebarPanel(
        actionButton("draw", "Draw samples"),
        numericInput("confidence", "Confidence (%):", 95, min = 1, step=1),
        numericInput("n", "Sample sizes:", 30, min = 1, step=1),
        numericInput("samples", "Number of confidence intervals:", 100, min = 1,
                     step=1),
        checkboxInput("sorted", "Sort confidence intervals by \\(\\bar{x}\\)",
                      FALSE),
        numericInput("mu", "mean (\\(\\mu\\))", 0),
        numericInput("sigma", "standard deviation (\\(\\sigma\\))", 1,
                     min = 0)
      ),

      mainPanel(
        plotOutput("plot"),
        textOutput("npct"),
        p("Blue line is the population mean \\(\\mu\\). ",
          "Black intervals do not contain\\(\\mu\\)."),
        withMathJax()
      )
    )
  )
)

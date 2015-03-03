
shinyUI(fluidPage(
  titlePanel("Normal Distribution"),

  sidebarLayout(
    sidebarPanel("",
                 selectInput("inputType", "Input Type?",
                             c("Quantile" = "q", "Probability" = "p")),
                 conditionalPanel("input.inputType == 'q'",
                                  numericInput("q", label = p("Quantile"),
                                               value = -1.96, step = 0.01)),
                 conditionalPanel("input.inputType == 'p'",
                                  numericInput("p", label = p("Cumulative Probability"),
                                               value = 0.025, step = 0.001,
                                               min = 0, max = 1)),
                 checkboxInput("lower.tail", "Lower Tail?",
                               value = TRUE),
                 p(""),
                 h3("Parameters"),
                 numericInput("mean", label = withMathJax("Mean (\\(\\mu\\))"),
                              value = 0, step = 0.01),
                 numericInput("sd", label = withMathJax("Std. Dev. (\\(\\sigma\\))"),
                              value = 1, step = 0.01, min = 0)
    ),
    mainPanel(plotOutput("plot"),
              uiOutput("table"))
  )
))

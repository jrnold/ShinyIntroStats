shinyUI(
  fluidPage(
    titlePanel("Significance Test of the Mean of a Normal Distribution"),
    sidebarLayout(
      sidebarPanel(
        h3("Hypothesis"),
        selectInput("direction","Direction of Hypothesis",
                    list("not equal to (!=)" = "neq",
                         "less than (<)" = "lt",
                         "greater than (>)" = "gt")),
        numericInput("mu", "Value of \\(\\mu\\)", 0),
        h3("Sample"),
        numericInput("n", "Sample size \\(n\\)", 30),
        numericInput("xbar", "Sample mean \\(\\bar{x}\\)",
                     round(1.96 / sqrt(30), 4)),
        numericInput("sigma", "Population std. dev. \\(\\sigma\\)", 1),
        withMathJax()
      ),
      mainPanel(
        strong("Hypotheses:"),
        uiOutput("H0"),
        uiOutput("Ha"),
        strong("Test Results:"),
        textOutput("z"),
        textOutput("pval"),
        plotOutput("plot"),
        helpText("Shaded area is the probability of observing a sample mean more",
                 "extreme than \\(\\bar x\\) under the alternative hypothesis",
                 "\\(H_a\\).")
      )
    )
  )
)

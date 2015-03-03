shinyUI(
  fluidPage(
    titlePanel("Hypothesis Test for a One-Sample Population Mean"),
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
        numericInput("sigma", "Standard deviation \\(s\\)", 1),
        checkboxInput("use_normal", "Use Normal distribution?", FALSE),
        helpText("This is equivalent to assuming that the sample standard deviation is exactly equal to the population standard deviation, \\(s = \\sigma\\)."),
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
        helpText("Shaded area is the probability of observing a test statistic more",
                 "extreme than the sample test statistic, assuming the null hypothesis is true.")
      )
    )
  )
)

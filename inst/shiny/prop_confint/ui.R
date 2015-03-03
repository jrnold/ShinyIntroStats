library(stringr)

shinyUI(
  fluidPage(
    titlePanel("Confidence Intervals of the Proportion"),

    #withMathJax(),
    sidebarLayout(

      sidebarPanel(
        actionButton("draw", "Draw samples"),
        numericInput("confidence", "Confidence (%):", 95, min = 1, max=99, step=1),
        numericInput("n", "Sample sizes:", 30, min = 1, step=1),
        numericInput("samples", "Number of confidence intervals:", 100, min = 1,
                     step=1),
        numericInput("p", "proportion (\\(p\\))", 0.5, min = 0, max = 1),
        checkboxInput("plus_four", "Plus four adjustment?", TRUE),
        helpText("Add 2 successes and 2 failures when calculating the sample proportion."),
        checkboxInput("sorted", "Sort confidence intervals by \\(\\hat{p}\\)",
                      FALSE)
      ),

      mainPanel(
        plotOutput("plot"),
        textOutput("npct"),
        p("Blue line is the population proportion \\(p\\). ",
          "Black intervals do not contain \\(p\\)."),
        p(str_c("The confidence intervals are calculated using ",
                "$$\\hat{p} \\pm z^* \\left( \\frac{\\hat{p} (1 - \\hat{p})}{n} \\right)$$ ",
                "where \\(z^*\\) is the critical value of the standard Normal distribution ",
                "such that the area \\(C\\)% is between \\(-z^*\\) and \\(z^*\\).")),
        withMathJax()
      )
    )
  )
)

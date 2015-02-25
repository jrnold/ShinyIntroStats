library("shiny")
shinyUI(fluidPage(
  h1("Draw a Simple Random Sample"),
  sidebarPanel(
    actionButton("submit", "Draw a new sample"),
    numericInput("n", "Number of Samples", 1),
    numericInput("min", "Minimum Number", 1),
    numericInput("max", "Maximum Number", 100),
    checkboxInput("replace", "Sample with replacement", value = FALSE)
  ),
  mainPanel(
    p(strong("Your sample is:")),
    textOutput("sample")
  )
))

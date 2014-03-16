library("shiny")
library("ggplot2")

shinyUI(bootstrapPage(
    h1("Student's t-distribution Density"),
    plotOutput("plot"),
    sliderInput(inputId = "df",
                label = "Degrees of Freedom",
                min = 1, max = 50, value = 1, step = 1,
                animate=animationOptions(interval=800, loop=TRUE))
))

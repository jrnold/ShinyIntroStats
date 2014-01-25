library("shiny")
library("ggplot2")

shinyUI(bootstrapPage(
   sliderInput(inputId = "df",
               label = "Degrees of Freedom",
               min = 1, max = 50, value = 1, step = 1,
               animate=animationOptions(interval=800, loop=T)),
   plotOutput("plot")

))

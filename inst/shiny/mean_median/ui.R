library("shiny")
library("ggvis")

shinyUI(fluidPage(
  titlePanel("Mean and Median"),
  sidebarLayout(
    sidebarPanel(numericInput("newval", "Add value:", 0, step = 1),
                     actionButton("update", "Update"),
                     actionButton("clear", "Clear Values")),
    mainPanel(ggvisOutput("plot"),
              textOutput("mean"),
              textOutput("median"),
              textOutput("values")
              ))))

library("shiny")
library("ggvis")

shinyUI(fluidPage(
  titlePanel("Mean and Median"),
  sidebarLayout(
    sidebarPanel(
        sidebarPanel(numericInput("newval", "Enter additional value", 0, step = 1),
                   actionButton("update", "Update"),
                   actionButton("clear", "Clear"))
    ),
    mainPanel(textOutput("stats"),
              textOutput("values")))))

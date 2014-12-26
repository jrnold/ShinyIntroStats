library("shiny")
library("ggvis")

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Data", dataTableOutput("table")),
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Histogram",
                 uiOutput("hist_ui"),
                 ggvisOutput("hist")
              ),
    tabPanel("Stem Plot", verbatimTextOutput("stem"))
  )
))

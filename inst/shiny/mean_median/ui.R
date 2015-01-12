library("stringr")
shinyUI(fluidPage(
  titlePanel("Mean and Median"),
  sidebarLayout(
    sidebarPanel(p(str_c("Enter data and see how ",
                         "measures of the center (mean, median) ",
                         "and spread (standard deviation, inter-quartile range) ",
                         "of the distribution change.")),
                 textInput("x", "Values:", "-1 0 1 3 5 8 7"),
                 helpText("A set of space-separated numbers."),
                 br(),
                 numericInput("binwidth", "dotplot binwidth", 1, step = 0.001)),
    mainPanel(plotOutput("plot"),
              p("Points represent the data. The purple line is the mean. The orange line is the median."),
              tableOutput("summary")
              ))))

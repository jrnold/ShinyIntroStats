library("ggvis")
shinyUI(fluidPage(
    h1("The Effect of Binwidth on Histograms"),
    ggvisOutput("ggvis"),
    uiOutput("ggvis_ui")
  ))

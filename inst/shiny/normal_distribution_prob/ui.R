shinyUI(fluidPage(
  titlePanel("Normal Distribution"),

  sidebarLayout(
    sidebarPanel("",
                 tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                            tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
                            tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
                 ),
                 selectInput("inputType", "Input Type?",
                             c("Quantile" = "q", "Probability" = "p")),
                 conditionalPanel("input.inputType == 'q'",
                                  numericInput("q", label = p("Quantile"),
                                               value = 0, step = 0.01)),
                 conditionalPanel("input.inputType == 'p'",
                                  numericInput("p", label = p("Cumulative Probability"),
                                               value = 0.5, step = 0.001, min = 0, max = 1)),
                 checkboxInput("lower.tail", "Lower Tail?",
                               value = TRUE),
                 p(""),
                 h3("Parameters"),
                 numericInput("mean", label = "Mean",
                              value = 0, step = 0.01),
                 numericInput("sd", label = "Std. Dev.",
                              value = 1, step = 0.01, min = 0)
    ),
    mainPanel("",
              plotOutput("plot"),
              uiOutput("table"))
  )
))

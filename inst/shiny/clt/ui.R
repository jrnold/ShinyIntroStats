#library("ggvis")
shinyUI(
  fluidPage(
    titlePanel("Sampling Distribution of Sample Means"),
    sidebarLayout(
      sidebarPanel(
        strong("Sample Size"),
        helpText("See what happens as the sample size increases"),
        sliderInput("obs", "", 0, 13, 0, step = 1,
                    animate=animationOptions(interval = 2^11, loop = FALSE),
                    pre = "2^"),
        helpText("Sample size in powers of 2."),
        helpText("How does the sampling distribution compare to a normal distribution?"),
        checkboxInput("draw_normal", "Draw Normal Distribution?", value = FALSE),
        strong("Distribution"),
        helpText("Try different population distributions"),
        selectInput("distr", "",
                    list("Normal" = "norm",
                         "Bernoulli" = "bernoulli",
                         "Beta" = "beta",
                         "Exponential" = "exp",
                         "Gamma" = "gamma",
                         "Uniform" = "unif")),
        conditionalPanel(
          condition = "input.distr == 'norm'",
          numericInput("norm_mean", "mean ", 0),
          numericInput("norm_sd", "sd > 0", 1, min = 0)
        ),
        conditionalPanel(
          condition = "input.distr == 'bernoulli'",
          numericInput("bernoulli_prob", "0 < p < 1", 0.5,
                       min = 0, max =1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.distr == 'beta'",
          numericInput("beta_shape1", "shape1 > 0", 0.5,
                       min = 0, step = 0.1),
          numericInput("beta_shape2", "shape2 > 0", 0.5,
                       min = 0, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.distr == 'exp'",
          numericInput("exp_rate", "rate > 0", 1,
                       min = 0, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.distr == 'gamma'",
          numericInput("gamma_shape", "shape > 0", 1,
                       min = 0, step = 0.1),
          numericInput("gamma_scale", "scale > 0", 1,
                       min = 0, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.distr == 'unif'",
          numericInput("unif_min", "Minimum", 0),
          numericInput("unif_max", "Maximum", 1)
        ),
        actionButton("action", "Resample")
      ),
      mainPanel(
        tableOutput("table"),
        plotOutput("plot")
        #ggvisOutput("ggvis")
      )
    )
  )
)


# ShinyIntroStats

Some [Shiny](http://shiny.rstudio.com/) apps appropriate for intro stats courses.

## Install

`ShinyIntroStats` is not available on CRAN.
You can install it from github using `devtools::install_github`.

```r
## install.packages("devtools")
install_github("jrnold/ShinyIntroStats")
```

## Usage

This package is primarily a means to distribute Shiny apps. To run one of the included apps, 

```r
library("ShinyIntroStats")
intro_stats_shinyapp()
```

Select the app that you would like to run and then Shiny will take care of the rest.

## Live Apps

Live versions of (some) of these apps are available on [shinyapps.io](http://shinyapps.io):

- Central limit simulations: https://jrnold.shinyapps.io/central_limit_theorem
- Population mean significance test: http://jrnold.shinyapps.io/significance_tests
- Confidence intervals of a mean: http://jrnold.shinyapps.io/prop_confint
- Confidence intervals of a proportion: http://jrnold.shinyapps.io/mean_confint
- Simple random number generator: https://jrnold.shinyapps.io/random
- Mean and median comparison: https://jrnold.shinyapps.io/mean_median
- The effect of binwidth on histograms: https://jrnold.shinyapps.io/histogram/
- Cumulative probabilities for the normal distribution: https://jrnold.shinyapps.io/normal_distribution_prob
- Comparison of means and standard deviations for normal distributions: https://jrnold.shinyapps.io/normal_distribution

## Miscellaneous Functions

Since far too much of introductory stats involves drawing normal or t-distributions and shading areas thereof, this package includes some convenience functions for drawing normal, t, and generic distributions and shading central or tail areas.



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

## Miscellaneous Functions

Since far too much of introductory stats involves drawing normal or t-distributions and shading areas thereof, this package includes some convenience functions for drawing normal, t, and generic distributions and shading central or tail areas.



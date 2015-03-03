#' Introductory Statistics Shiny Apps
#'
#' @name ShinyIntroStats
#' @docType package
#' @import shiny
NULL

.shiny_apps <-
    c(clt = "Central Limit Theorem",
      tdist = "t-distribution",
      confint = "Confidence interval coverage",
      mean_confint = "Confidence intervals (mean)",
      prop_confint = "Confidence intervals (proportion)"
      )

run_shiny_app <- function(name, ...) {
  shiny::runApp(system.file(file.path("shiny", name),
                            package="ShinyIntroStats"), ...)
}

#' Run Intro Stats Shiny Apps
#'
#' Run the Shiny Apps included in this package locally.
#'
#' @export
intro_stats_shinyapp <- function() {
    prompt <- paste0("Enter the number of the app to run:\n",
                     paste(sprintf("%d. %s",
                                   seq_along(.shiny_apps),
                                   .shiny_apps),
                           collapse = "\n"),
                     "\n")
    choice <- readline(prompt)
    if (! choice %in% as.character(seq_along(.shiny_apps))) {
        choice <- readline(sprintf("Enter a number 1-%d:\n", length(.shiny_apps)))
    } else {
        run_shiny_app(names(.shiny_apps)[as.integer(choice)])
    }
}

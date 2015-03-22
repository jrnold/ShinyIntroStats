#' Introductory Statistics Shiny Apps
#'
#' @name ShinyIntroStats
#' @docType package
#' @import shiny
#' @import yaml
NULL

.SHINY_APPS <-
  basename(Filter(function(x) file_test("-d", x),
           dir(system.file("shiny", package = "ShinyIntroStats"),
               full.names = TRUE)))

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
                                   seq_along(.SHINY_APPS),
                                   .SHINY_APPS),
                           collapse = "\n"),
                     "\n\n")
    choice <- readline(prompt)
    if (! choice %in% as.character(seq_along(.SHINY_APPS))) {
        choice <- readline(sprintf("Enter a number 1-%d:\n", length(.SHINY_APPS)))
    } else {
        run_shiny_app(.SHINY_APPS[as.integer(choice)])
    }
}

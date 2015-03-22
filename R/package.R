#' Introductory Statistics Shiny Apps
#'
#' @name ShinyIntroStats
#' @docType package
#' @import shiny
#' @import yaml
NULL

shiny_app_list <- function() {
  apps <- Filter(function(x) file_test("-d", x),
                 dir(system.file("shiny", package = "ShinyIntroStats"),
                     full.names = TRUE))
  ret <- lapply(file.path(apps, "metadata.yaml"),
                yaml.load_file)
  names(ret) <- basename(apps)
  ret
}

run_shiny_app <- function(name, ...) {
  shiny::runApp(system.file(file.path("shiny", name),
                            package="ShinyIntroStats"), ...)
}

#' Run Intro Stats Shiny Apps
#'
#' Run the Shiny Apps included in this package locally.
#'
#' @export
intro_stats_shinyapps <- function() {
  apps <- shiny_app_list()
  cat(paste0("Enter the number of the app to run:\n",
                   paste(sprintf("%d. %s",
                                 seq_along(apps),
                                 sapply(apps, function(x) x[["title"]])),
                         collapse = "\n")),
                   "\n\n")
  choice <- readline("App number: ")
  while (! choice %in% as.character(seq_along(apps))) {
    choice <- readline(sprintf("Enter a number 1-%d:\n", length(apps)))
  }
  run_shiny_app(names(apps)[as.integer(choice)])
}

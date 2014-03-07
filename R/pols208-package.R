#' POLS208
#'
#' Code and data for POLS 208: Political Methods, Emory University, Spring 2014.
#'
#' @name pols208
#' @docType package
#' @import shiny
NULL

run_shiny_app <- function(name, ...) {
    runApp(system.file(file.path("inst", "shiny", name), package="pols208"), ...)
}

#' @export
shiny_run_clt <- function(...) run_shiny_app("clt", ...)

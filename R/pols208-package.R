#' POLS208
#'
#' Code and data for POLS 208: Political Methods, Emory University, Spring 2014.
#'
#' @name pols208
#' @docType package
#' @import shiny
NULL

.shiny_apps <-
    c(clt = "Central Limit Theorem",
      tdist = "t-distribution",
      ci = "Confidence intervals"
      )


choose_shiny_app <- function() {
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


run_shiny_app <- function(name, ...) {
    shiny::runApp(system.file(file.path("shiny", name), package="pols208"), ...)
}

#' @export
shiny_run_clt <- function(...) run_shiny_app("clt", ...)

#' @export
shiny_run_tdist <- function(...) run_shiny_app("tdist", ...)

#' @export
shiny_run_confint <- function(...) run_shiny_app("confint", ...)

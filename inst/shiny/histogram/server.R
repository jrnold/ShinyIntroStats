library("shiny")
library("ggvis")
library("bps6data")

data("iowatest")
shinyServer(function(input, output) {
  (iowatest %>%
    ggvis(~ Score) %>%
    layer_histograms(width = input_slider(0.05, 13, value = 0.5, step = 0.1, label = "Histogram bin width")) %>%
    layer_density(width = input_slider(0.05, 13, value = 0.5, setp = 0.1, label = ""))
    add_axis("x", title = "Iowa Test Vocabulary Score") %>%
    add_axis("y", title = "% of 7th grade students") %>%
    bind_shiny("ggvis", "ggvis_ui"))
})

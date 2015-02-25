library("ggplot2")
library("stringr")

formatValues <- function(x) {
  if (is.na(x) || is.nan(x)) ""
  else prettyNum(x, format = "g", digits = 3)
}

split2numeric <- function(x) {
  x <- str_replace_all(str_trim(x), ',', '')
  x <- str_replace_all(x, '[^0-9.-]+', ' ')
  as.numeric(str_split(x, ' +')[[1]])
}

shinyServer(function(input, output) {

  x <- reactive({
    split2numeric(input$x)
  })

  xmean <- reactive(mean(x()))
  xmedian <- reactive(median(x()))
  xsd <- reactive(sd(x()))
  xIQR <- reactive(IQR(x()))

  output$plot <-
    renderPlot({
      binwidth = 1
      (ggplot(data.frame(x = x()),
             aes(x = x))
       + geom_dotplot(binwidth = input$binwidth)
       + geom_vline(xintercept = xmean(), colour = "purple")
       + geom_vline(xintercept = xmedian(), colour = "orange")
       + scale_y_continuous(limits = c(0, 1))
       + scale_x_continuous("")
       )
    })
  output$summary <-
    renderTable({
      x1 <- c("Mean", "Median")
      x3 <- c("Std. dev", "IQR")
      x2 <- sapply(c(xmean(), xmedian()), formatValues)
      x4 <- sapply(c(xsd(), xIQR()), formatValues)
      cbind(x1, x2, x3, x4)
    }, include.rownames = FALSE, include.colnames = FALSE
    )

})


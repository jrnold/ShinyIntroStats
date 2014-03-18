library("shiny")
library("ggplot2")
library("plyr")

shinyServer(function(input, output) {
    output$plot <- renderPlot({
        df1 <- input$n1 - 1
        df2 <- input$n2 - 1
        t1 <- -qt((100 - input$conf1) / 200, df = df1)
        t2 <- -qt((100 - input$conf2) / 200, df = df2)
        se1 <- input$s1 / sqrt(input$n1)
        se2 <- input$s2 / sqrt(input$n2)
                               
        df <- data.frame(i = factor(c(1, 2)),
                         y = c(input$xbar1, input$xbar2),
                         ymin = c(input$xbar1, input$xbar2) - c(t1, t2) * c(se1, se2),
                         ymax = c(input$xbar1, input$xbar2) + c(t1, t2) * c(se1, se2))

       print(ggplot(df, aes(x = i, y = y, ymin = ymin, ymax = ymax))
             + geom_pointrange()
             + scale_x_discrete(""))
    })
})

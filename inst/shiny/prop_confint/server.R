library("shiny")
library("ggplot2")
library("plyr")

shinyServer(function(input, output) {
    output$plot <- renderPlot({
       se1 <- sqrt(input$p1 * (1 - input$p1) / sqrt(input$n1))
       z1 <- -qnorm((100 - input$conf1) / 200)
       se2 <- sqrt(input$p1 * (1 - input$p2) / sqrt(input$n2))    
       z2 <- -qnorm((100 - input$conf2) / 200)

       df <- data.frame(i = factor(c(1, 2)),
                        y = c(input$p1, input$p2),
                        ymin = c(input$p1, input$p2) - c(z1, z2) * c(se1, se2),
                        ymax = c(input$p1, input$p2) + c(z1, z2) * c(se1, se2))

       print(ggplot(df, aes(x = i, y = y, ymin = ymin, ymax = ymax))
             + geom_pointrange()
             + scale_x_discrete(""))
    })
})

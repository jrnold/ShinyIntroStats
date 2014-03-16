library("shiny")

x <- seq(-4, 4, by=0.01)
norm_dens <- dnorm(x)

shinyServer(function(input, output) {

    output$plot <- renderPlot({
        t_dens <- dt(x, df = input$df)
        print(ggplot()
              + geom_line(data = data.frame(x = x, y = norm_dens),
                          mapping = aes(x = x, y = y), colour = "blue")
              + geom_line(data = data.frame(x = x, y = t_dens),
                          mapping = aes(x = x, y = y), colour = "red")
              + scale_x_continuous("x")
              + scale_y_continuous("p(x)")
              + ggtitle(sprintf("degrees of freedom = %d", input$df)))
    })
})

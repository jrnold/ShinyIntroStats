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
              + ggtitle(sprintf("Student t-distribution (df = %d)", input$df)))
    })
})

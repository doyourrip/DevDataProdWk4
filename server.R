# This is a shiny app to determine diamond price based on factors Carat, Cut and Colour

library(shiny)
library(ggplot2)
library(curl)
library(datasets)

# Server logic
shinyServer(function(input, output) {
  
  # import data
  data("diamonds")
  
  # create plot
  output$distPlot <- renderPlot({
    
    # subset the data
    diamonds_sub <- subset(diamonds, cut == input$cut &
                             color == input$color)
    
    # plot the data for carat and price
    p <- ggplot(data = diamonds_sub, aes(x = carat, y = price)) + geom_point(aes(colour = price))
    p <- p + geom_smooth(method = "lm") + xlab("Carat") + ylab("Price")
    p <- p + xlim(0, 6) + ylim (0, 20000)
    p
  }, height = 700)
  
  # create model
  output$predict <- renderPrint({
    diamonds_sub <- subset( diamonds, cut == input$cut &
                              color == input$color)
    fit <- lm(price~carat,data=diamonds_sub)
    unname(predict(fit, data.frame(carat = input$lm)))})
  
  observeEvent(input$predict, {distPlot <<- NULL
  output$distPlot <- renderPlot({p <- ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point(aes(colour = price))
  p <- p + geom_smooth(method = "lm") + xlab("Carat") + ylab("Price")
  p <- p + xlim(0, 6) + ylim (0, 20000)
  p
  }, height = 700)})})
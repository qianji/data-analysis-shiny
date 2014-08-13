library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# load data


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({
    #industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
    d<- load('data/industry_sum.Rdata')
    industry_sum <- get(d[1])
    data <- industry_sum
    if (input$indus != "All"){
      data <- data[data$INDUS == input$indus,]
    }
    if (input$members != "All"){
      data <- data[data$Members == input$members,]
    }
    if (input$rankPos != "All"){
      data <- data[data$RankPos == input$rankPos,]
    }
    data
  })
  
})

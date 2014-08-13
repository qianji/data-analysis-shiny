library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

#load data
d  <- load('data/industry_sum.Rdata')
industry_sum <- get(d[1])
#industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Industry Sum Data"),
          
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
          selectInput("indus", 
                      "INDUS:", 
                      c("All", 
                        unique(as.character(industry_sum$INDUS))))
      ),
      column(4, 
          selectInput("members", 
                      "Members:", 
                      c("All", 
                        unique(as.character(industry_sum$Members))))
      ),
      column(4, 
          selectInput("rankPos", 
                      "RankPos", 
                      c("All", 
                        unique(as.character(industry_sum$RankPos))))
      )        
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    )    
  )  
)

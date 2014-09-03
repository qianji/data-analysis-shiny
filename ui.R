library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

#load data
d  <- load('data/subsector_sum.Rdata')
industry_sum <- get(d[1])
#industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
# Define the overall UI
shinyUI(

  fluidPage(
    titlePanel("Data Analysis"),

    # side bar of the page
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "INDUS"',
        selectInput("industry_indus", 
                      "INDUS:", 
                      c("All", 
                        unique(as.character(industry_sum$INDUS))))
      ),
      conditionalPanel(
        'input.dataset === "SUB.SEC"',
            selectInput("sub_indus", 
                        "INDUS:", 
                        c("All", 
                          unique(as.character(industry_sum$INDUS)))),
            selectInput("sub_sup_sec", 
                        "SUP.SEC:", 
                        c("All", 
                          unique(as.character(industry_sum$SUP.SEC)))),
            selectInput("sub_sec", 
                        "SEC", 
                        c("All", 
                          unique(as.character(industry_sum$SEC)))),
            selectInput("sub_sub_sec", 
                        "SUB.SEC", 
                        c("All", 
                          unique(as.character(industry_sum$SUB.SEC))))

      ),
      conditionalPanel(
        'input.dataset === "SUP.SEC"',
            selectInput("sup_indus", 
                        "INDUS:", 
                        c("All", 
                          unique(as.character(industry_sum$INDUS)))),
            selectInput("sup_sup_sec", 
                        "SUP.SEC:", 
                        c("All", 
                          unique(as.character(industry_sum$SUP.SEC))))

      ),

      conditionalPanel(
        'input.dataset === "SEC"',
            selectInput("sec_indus", 
                        "INDUS:", 
                        c("All", 
                          unique(as.character(industry_sum$INDUS)))),
            selectInput("sec_sup_sec", 
                        "SUP.SEC:", 
                        c("All", 
                          unique(as.character(industry_sum$SUP.SEC)))),
            selectInput("sec_sec", 
                        "SEC", 
                        c("All", 
                          unique(as.character(industry_sum$SEC))))

      )

      
    ),

    mainPanel(tabsetPanel(id='dataset',tabPanel('INDUS',dataTableOutput('industry')), tabPanel('SUP.SEC',dataTableOutput('sup_sec')), tabPanel('SEC',dataTableOutput('sec')),tabPanel('SUB.SEC',dataTableOutput('sub_sec')) ))
    
 
  )  
)

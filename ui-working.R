library(shiny)
library(ggplot2)


nyse <- load('data/subsector_sum.RData')
#nasdaq <- load('data/sector_sum_nasdaq.RData')
#other <- load('data/subsector_sum_special.RData')

industry_sum <- get(nyse[1])
#industry_sum_nasdaq <- get(nasdaq[1])

shinyUI(
  navbarPage("Groups",
             tabPanel("NYSE",
                      
                      icon=icon("table")),
             tabPanel("NASDAQ",icon=icon("table")),
             tabPanel("Other",icon=icon('table')),
             
             conditionalPanel(
               'input.dataset === "INDUS"',
               fluidRow(column(6,selectInput("industry_indus", "Industry",
                                              c("All", unique(as.character(industry_sum$INDUS))),
                                             selected="All", selectize=FALSE,multiple=TRUE)))),
             conditionalPanel(
                'input.dataset === "SUP.SEC"',
                fluidRow(column(6,selectInput("sup_indus", "Industry", 
                                                 c("All", unique(as.character(industry_sum$INDUS))),
                                              selected="All", selectize=FALSE,multiple=TRUE)),
                         column(6,selectInput("sup_sup_sec", "Super Sector",
                                                 c("All", unique(as.character(industry_sum$SUP.SEC))),
                                              selected="All", selectize=FALSE,multiple=TRUE)))),
             conditionalPanel(
               'input.dataset === "SEC"',
               fluidRow(column(4,selectInput("sec_indus", "Industry", 
                                             c("All", unique(as.character(industry_sum$INDUS))),
                                             selected="All",selectize=FALSE,multiple=TRUE)),
                        column(4,selectInput("sec_sup_sec", "Super Sector",
                                             c("All", unique(as.character(industry_sum$SUP.SEC))),
                                             selected="All", selectize=FALSE,multiple=TRUE)),
                        column(4,selectInput("sec_sec", "Sector",
                                             c("All", unique(as.character(industry_sum$SEC))),
                                             selected="All", selectize=FALSE,multiple=TRUE)))),
             conditionalPanel(
               'input.dataset === "SUB.SEC"',
               fluidRow(column(3,selectInput("sub_indus", "Industry", 
                                             c("All", unique(as.character(industry_sum$INDUS))),
                                             selected="All",selectize=FALSE,multiple=TRUE)),
                        column(3,selectInput("sub_sup_sec", "Super Sector",
                                             c("All", unique(as.character(industry_sum$SUP.SEC))),
                                             selected="All", selectize=FALSE,multiple=TRUE)),
                        column(3,selectInput("sub_sec", "Sector",
                                             c("All", unique(as.character(industry_sum$SEC))),
                                             selected="All", selectize=FALSE,multiple=TRUE)),
                        column(3,selectInput("sub_sub_sec", "Sub Sector",
                                             c("All", unique(as.character(industry_sum$SUB.SEC))),
                                             selected="All", selectize=FALSE,multiple=TRUE)))),                         
             
             fluidRow(column(12,mainPanel(tabsetPanel(id='dataset',
                                                      tabPanel('INDUS',dataTableOutput('industry')), 
                                                      tabPanel('SUP.SEC',dataTableOutput('sup_sec')), 
                                                      tabPanel('SEC',dataTableOutput('sec')),
                                                      tabPanel('SUB.SEC',dataTableOutput('sub_sec'))
                                                      )))),
             header=NULL,
             footer=NULL,
             fluid=TRUE,
             inverse=TRUE,
             collapsable=FALSE,
             responsive=TRUE,
             #theme="css/style.css",
             navbarMenu("Modes",
                        #radioButtons("mode","Mode",c("Snapshot"="snap",
                        #                           "Trend"="trend",
                        #                           "Plots"="plot",
                        #                           "Map"="map"),
                        #             selected="snap")
                        tabPanel("Snapshot",icon=icon("camera")),
                        tabPanel("Trend",icon=icon("align-left")),
                        tabPanel("Plots",icon=icon("bar-chart-o"))
             )
             
             #                conditionalPanel(
             #                  'input.dataset === "SUB.SEC"',
             #                  column(width=3,
             #                         selectInput("sub_indus", 
             #                                     "INDUS:", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$INDUS)))),
             #                  ),
             #                  column(width=3,
             #                         selectInput("sub_sup_sec", 
             #                                     "SUP.SEC:", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$SUP.SEC)))),
             #                  ),
             #                  column(width=3,
             #                         selectInput("sub_sec", 
             #                                     "SEC", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$SEC)))),
             #                  ),
             #                  column(width=3,
             #                         selectInput("sub_sub_sec", 
             #                                     "SUB.SEC", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$SUB.SEC))))
             #                         
             #                  )),

             #                conditionalPanel(
             #                  'input.dataset === "SEC"',
             #                  column(width=4,
             #                         selectInput("sec_indus", 
             #                                     "INDUS:", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$INDUS)))),
             #                  ),
             #                  column(width=4,
             #                         selectInput("sec_sup_sec", 
             #                                     "SUP.SEC:", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$SUP.SEC)))),
             #                  ),
             #                  column(width=4,
             #                         selectInput("sec_sec", 
             #                                     "SEC", 
             #                                     c("All", 
             #                                       unique(as.character(industry_sum$SEC))))
             #                         
             #                  ))
             #             ), # fluidRow
             # fluidRow(
             #   column(12,
             #          mainPanel(tabsetPanel(id='dataset',
             #                                tabPanel('INDUS',dataTableOutput('industry')), 
             #                                tabPanel('SUP.SEC',dataTableOutput('sup_sec')), 
             #                                tabPanel('SEC',dataTableOutput('sec')),
             #                                tabPanel('SUB.SEC',dataTableOutput('sub_sec')) ))
             #   )) # fluidRow
             #              
             
  )  # navbar page
)

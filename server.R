
library(shiny)
library(dplyr)
library(ggplot2)

# constants
DECIMALS <- 2

# not-in-set operator
`%nin%` <- Negate(`%in%`) 

# server
shinyServer(function(input, output, session) {

  # load a data store and return its first element
#   load_data_frame <- function(filename) {
#     d <- load(filename)
#     d <- get(d[1])
#     nums <- sapply(d,is.numeric)
#     d[,nums] <- round(d[,nums],DECIMALS)
#     d
#   }
  
  # reactive data acquisition for the session
  load_dataset <- reactive({
    # loads enviroment 'dataset' from group-power into global environment
    load("data/group-report.RData",envir=.GlobalEnv)
  })
  
  ### NYSE industry panel
  output$nyse_industry_menu <- renderUI({
    load_dataset()
    d <- dataset$nyse_industry_sum
    selectInput("nyse_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$industry <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_industry_sum %>%
      select(INDUS,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_industry_menu ) {
      data <- filter(data,INDUS %in% input$nyse_industry_menu)
    }
    data
  })
  
  
  ### NYSE supersector panel
  output$nyse_sup_indus <- renderUI({
    load_dataset()
    d <- dataset$nyse_supsector_sum
    selectInput("nyse_sup_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sup_sup <- renderUI({
    load_dataset()
    d <- dataset$nyse_supsector_sum
    selectInput("nyse_sup_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sup_industry_selection <- input$nyse_sup_indus 
    if ( length(sup_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_supsector_sum
      d <- filter(d,INDUS==sup_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sup_sup", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  output$sup_sec <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_supsector_sum %>%
      select(INDUS,SUP.SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sup_indus ) {
      data <- filter(data,INDUS %in% input$nyse_sup_indus)
    }
    if ( "All" %nin% input$nyse_sup_sup ) {
      data <- filter(data,SUP.SEC %in% input$nyse_sup_sup)
    }
    data
  })
  
  ### NYSE sector panel
  output$nyse_sec_indus <- renderUI({
    load_dataset()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sec_sup <- renderUI({
    load_dataset()
    # d <- data_sector_sum()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sec_sec <- renderUI({
    load_dataset()
    # d <- data_sector_sum()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$nyse_sec_indus
    if ( length(sec_industry_selection) > 0 ) {
      load_dataset()
      # d <- data_sector_sum()
      d <- dataset$nyse_sector_sum
      d <- filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sec_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sec_sup_selection <- input$nyse_sec_sup
    if ( length(sec_sup_selection) > 0 ) {
      load_dataset()
      # d <- data_sector_sum()
      d <- dataset$nyse_sector_sum
      d <- d %>% filter(SUP.SEC==sec_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "nyse_sec_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$sec <- renderDataTable({
    # data <- data_sector_sum()
    load_dataset()
    data <- dataset$nyse_sector_sum %>%
      select(INDUS,SUP.SEC,SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sec_indus ) {
      data <- filter(data,INDUS %in% input$nyse_sec_indus)
    }
    if ( "All" %nin% input$nyse_sec_sup ) {
      data <- filter(data,SUP.SEC %in% input$nyse_sec_sup)
    }
    if ( "All" %nin% input$nyse_sec_sec ) {
      data <- filter(data,SEC %in% input$nyse_sec_sec)
    }
    
    data
  })
  
  ### NYSE subsector panel
  output$nyse_sub_indus <- renderUI({
    #d <- data_subsector_sum()
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sup <- renderUI({
    # d <- data_subsector_sum()
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sec <- renderUI({
    #d <- data_subsector_sum()
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sub <- renderUI({
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sub", "Sub Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sub_industry_selection <- input$nyse_sub_indus
    if ( length(sub_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- filter(d,INDUS==sub_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sub_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sup_selection <- input$nyse_sub_sup
    if ( length(sub_sup_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- d %>% filter(SUP.SEC==sub_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "nyse_sub_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sec_selection <- input$nyse_sub_sec
    if ( length(sub_sec_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- d %>% filter(SEC==sub_sec_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "nyse_sub_sub", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$sub_sec <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_subsector_sum %>%
      select(INDUS,SUP.SEC,SEC,SUB.SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sub_indus ) {
      data <- filter(data,INDUS %in% input$nyse_sub_indus)
    }
    if ( "All" %nin% input$nyse_sub_sup ) {
      data <- filter(data,SUP.SEC %in% input$nyse_sub_sup)
    }
    if ( "All" %nin% input$nyse_sub_sec ) {
      data <- filter(data,SEC %in% input$nyse_sub_sec)
    }
    if ( "All" %nin% input$nyse_sub_sub ) {
      data <- filter(data,SUB.SEC %in% input$nyse_sub_sub)
    }
    data
  }
  )
  
  ####### NASDAQ industry
  output$nasdaq_industry_menu <- renderUI({
    load_dataset()
    d <- dataset$nasdaq_industry_sum
    selectInput("nasdaq_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nasdaq_industry <- renderDataTable({
    load_dataset()
    data <- dataset$nasdaq_industry_sum %>%
      select(INDUS,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nasdaq_industry_menu ) {
      data <- filter(data,INDUS %in% input$nasdaq_industry_menu) 
    }
    data
  }
  )
  
  ###### NASDAQ sector
  
  output$nasdaq_sec_indus <- renderUI({
    load_dataset()
    d <- dataset$nasdaq_sector_sum
    selectInput("nasdaq_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nasdaq_sec_sec <- renderUI({
    #d <- data_sector_sum_nasdaq()
    load_dataset()
    d <- dataset$nasdaq_sector_sum
    selectInput("nasdaq_sec_sec", "Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$nasdaq_sec_indus 
    if ( length(sec_industry_selection) > 0 ) {
      #d <- data_sector_sum_nasdaq()
      load_dataset()
      d <- dataset$nasdaq_sector_sum
      d <- filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "nasdaq_sec_sec", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  
  output$nasdaq_sec <- renderDataTable({
    #data <- data_sector_sum_nasdaq()
    load_dataset()
    data <- dataset$nasdaq_sector_sum %>%
      select(INDUS,SUB.SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    if ( length(input$nasdaq_sec_indus) > 0) {
      if ( "All" %nin% input$nasdaq_sec_indus ) {
        data <- filter(data,INDUS %in% input$nasdaq_sec_indus)
      }
    }
    if ( length(input$nasdaq_sec_sec) > 0) {
      if ( "All" %nin% input$nasdaq_sec_sec ) {
        data <- filter(data,SUB.SEC %in% input$nasdaq_sec_sec)
      }
    }
    data
  }
  )
  
  ##### ETPs
  
  ####### ETP normal
  output$etp_normal_menu <- renderUI({
    load_dataset()
    d <- dataset$etf_normal
    selectInput("etf_normal_menu", "Normal",
                c("All", unique(as.character(d$ETFdb.Category))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$etp_normal <- renderDataTable({
    load_dataset()
    data <- dataset$etf_normal %>%
      select(ETFdb.Category,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    if ( "All" %nin% input$etf_normal_menu ) {
      data <- filter(data,ETFdb.Category %in% input$etf_normal_menu) 
    }
    data
  }
  )
  
  ####### ETP inverse
  output$etp_inverse_menu <- renderUI({
    load_dataset()
    d <- dataset$etf_inverse
    selectInput("etf_inverse_menu", "Inverse",
                c("All", unique(as.character(d$ETFdb.Category))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$etp_inverse <- renderDataTable({
    load_dataset()
    data <- dataset$etf_inverse %>% 
      select(ETFdb.Category,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    if ( "All" %nin% input$etf_inverse_menu ) {
      data <- filter(data,ETFdb.Category %in% input$etf_inverse_menu) 
    }
    data
  }
  )
  
  
  ##### SPECIAL
  
  ### SPECIAL industry panel
  output$special_industry_menu <- renderUI({
    #d <- data_industry_sum_special()
    load_dataset()
    d <- dataset$special_industry_sum
    selectInput("special_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_industry <- renderDataTable({
    # data <- data_industry_sum_special()
    load_dataset()
    data <- dataset$special_industry_sum %>%
      select(INDUS,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_industry_menu) > 0 ) {
      if ( "All" %nin% input$special_industry_menu ) {
        data <- filter(data,INDUS %in% input$special_industry_menu)
      }
    }
    data
  })
  
  
  ### SPECIAL supersector panel
  output$special_sup_indus <- renderUI({
    #d <- data_supsector_sum_special()
    load_dataset()
    d <- dataset$special_supsector_sum
    selectInput("special_sup_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sup_sup <- renderUI({
    # d <- data_supsector_sum_special()
    load_dataset()
    d <- dataset$special_supsector_sum
    selectInput("special_sup_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sup_industry_selection <- input$special_sup_indus 
    if ( length(sup_industry_selection) > 0 ) {
      # d <- data_supsector_sum_special()
      load_dataset()
      d <- dataset$special_supsector_sum
      d <- filter(d,INDUS==sup_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sup_sup", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  output$special_sup_sec <- renderDataTable({
    # data <- data_supsector_sum_special()
    load_dataset()
    data <- dataset$special_supsector_sum %>%
      select(INDUS,SUP.SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_sup_indus) > 0 ) {
      if ( "All" %nin% input$special_sup_indus ) {
        data <- filter(data,INDUS %in% input$special_sup_indus)
      }
    }
    if ( length(input$special_sup_sup) > 0 ) {
      if ( "All" %nin% input$special_sup_sup ) {
        data <- filter(data,SUP.SEC %in% input$special_sup_sup)
      }
    }
    data
  })
  
  ### SPECIAL sector panel
  output$special_sec_indus <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sec_sup <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sec_sec <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$special_sec_indus
    if ( length(sec_industry_selection) > 0 ) {
      #d <- data_sector_sum_special()
      load_dataset()
      d <- dataset$special_sector_sum
      d <- filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sec_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sec_sup_selection <- input$special_sec_sup
    if ( length(sec_sup_selection) > 0 ) {
      # d <- data_sector_sum_special()
      load_dataset()
      d <- dataset$special_sector_sum
      d <- d %>% filter(SUP.SEC==sec_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "special_sec_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$special_sec <- renderDataTable({
    # data <- data_sector_sum_special()
    load_dataset()
    data <- dataset$special_sector_sum %>%
      select(INDUS,SUP.SEC,SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_sec_indus) > 0) {
      if ( "All" %nin% input$special_sec_indus ) {
        data <- filter(data,INDUS %in% input$special_sec_indus)
      }
    }
    if ( length(input$special_sec_sup) > 0) {
      if ( "All" %nin% input$special_sec_sup ) {
        data <- filter(data,SUP.SEC %in% input$special_sec_sup)
      }
    }
    if ( length(input$special_sec_sec)>0) {
      if ( "All" %nin% input$special_sec_sec ) {
        data <- filter(data,SEC %in% input$special_sec_sec)
      }
    }
    
    data
  })
  
  ### SPECIAL subsector panel
  
  output$special_sub_indus <- renderUI({
    # d <- data_subsector_sum_special()
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sup <- renderUI({
    #d <- data_subsector_sum_special()
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sec <- renderUI({
    #d <- data_subsector_sum_special()
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sub <- renderUI({
    #d <- data_subsector_sum_special()
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sub", "Sub Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sub_industry_selection <- input$special_sub_indus
    if ( length(sub_industry_selection) > 0 ) {
      #d <- data_subsector_sum_special()
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- filter(d,INDUS==sub_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sub_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sup_selection <- input$special_sub_sup
    if ( length(sub_sup_selection) > 0 ) {
      #d <- data_subsector_sum_special()
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- d %>% filter(SUP.SEC==sub_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "special_sub_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sec_selection <- input$special_sub_sec
    if ( length(sub_sec_selection) > 0 ) {
      #d <- data_subsector_sum_special()
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- d %>% filter(SEC==sub_sec_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "special_sub_sub", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$special_subsec_table <- renderDataTable({
    # data <- data_subsector_sum_special()
    load_dataset()
    data <- dataset$special_subsector_sum %>%
      select(INDUS,SUP.SEC,SEC,SUB.SEC,Members,HILO,PChg,ShortRank,IntRank,LongRank,PSMF,RankPos,Gear) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_sub_indus) > 0) {
      if ( "All" %nin% input$special_sub_indus ) {
        data <- filter(data,INDUS %in% input$special_sub_indus)
      }
    }
    if ( length(input$special_sub_sup) > 0) {
      if ( "All" %nin% input$special_sub_sup ) {
        data <- filter(data,SUP.SEC %in% input$special_sub_sup)
      }
    }
    if ( length(input$special_sub_sec) > 0) {
      if ( "All" %nin% input$special_sub_sec ) {
        data <- filter(data,SEC %in% input$special_sub_sec)
      }
    }
    if ( length(input$special_sub_sub) > 0) {
      if ( "All" %nin% input$special_sub_sub ) {
        data <- filter(data,SUB.SEC %in% input$special_sub_sub)
      }
    }
    data
  }
  )
  
  
})
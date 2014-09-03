library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# load data


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$industry <- renderDataTable({
    #industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
    d<- load('data/industry_sum.Rdata')
    industry_sum <- get(d[1])
    data <- industry_sum
    if (input$industry_indus != "All"){
      data <- data[data$INDUS == input$industry_indus,]
    }
    data
  }, 
  
  #options = list (bSortClasses = TRUE)  

  options = list(fnRowCallback = I('
  function(nRow, aData,iDisplayIndex, iDisplayIndexFull) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(aData[18]) <= 3.0)
      $("td:eq(18)", nRow).css("color", "Green");
    else if (parseFloat(aData[14]) <= 6.0)
      $("td:eq(18)", nRow).css("color", "Yellow");
    else if (parseFloat(aData[14]) <= 9.0)
      $("td:eq(18)", nRow).css("color", "Blue");
    else 
      $("td:eq(18)", nRow).css("color", "Red");


  }'
  ))
  
  )


  # output for super sector
  output$sup_sec <- renderDataTable({
    #industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
    d<- load('data/supsector_sum.Rdata')
    industry_sum <- get(d[1])
    data <- industry_sum
    if (input$sup_indus != "All"){
      data <- data[data$INDUS == input$sup_indus,]
    }
    if (input$sup_sup_sec != "All"){
      data <- data[data$SUP.SEC == input$sup_sup_sec,]
    }
    data
  }, 
  
  #options = list (bSortClasses = TRUE)  

  options = list(fnRowCallback = I('
  function(nRow, aData,iDisplayIndex, iDisplayIndexFull) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(aData[18]) <= 3.0)
      $("td:eq(18)", nRow).css("color", "Green");
    else if (parseFloat(aData[14]) <= 6.0)
      $("td:eq(18)", nRow).css("color", "Yellow");
    else if (parseFloat(aData[14]) <= 9.0)
      $("td:eq(18)", nRow).css("color", "Blue");
    else 
      $("td:eq(18)", nRow).css("color", "Red");


  }'
  ))
  
  )


    # output for sector
  output$sec <- renderDataTable({
    #industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
    d<- load('data/sector_sum.Rdata')
    industry_sum <- get(d[1])
    data <- industry_sum
    if (input$sec_indus != "All"){
      data <- data[data$INDUS == input$sec_indus,]
    }
    if (input$sec_sup_sec != "All"){
      data <- data[data$SUP.SEC == input$sec_sup_sec,]
    }
    if (input$sec_sec != "All"){
      data <- data[data$SUP.SEC == input$sec_sec,]
    }

    data
  }, 
  
  #options = list (bSortClasses = TRUE)  

  options = list(fnRowCallback = I('
  function(nRow, aData,iDisplayIndex, iDisplayIndexFull) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(aData[18]) <= 3.0)
      $("td:eq(18)", nRow).css("color", "Green");
    else if (parseFloat(aData[14]) <= 6.0)
      $("td:eq(18)", nRow).css("color", "Yellow");
    else if (parseFloat(aData[14]) <= 9.0)
      $("td:eq(18)", nRow).css("color", "Blue");
    else 
      $("td:eq(18)", nRow).css("color", "Red");


  }'
  ))
  
  )



  # output for sub sector
  output$sub_sec <- renderDataTable({
    #industry_sum <- reactive(get(load('data/industry_sum.Rdata'))[1])
    d<- load('data/subsector_sum.Rdata')
    industry_sum <- get(d[1])
    data <- industry_sum
    if (input$sub_indus != "All"){
      data <- data[data$INDUS == input$sub_indus,]
    }
    if (input$sub_sup_sec != "All"){
      data <- data[data$SUP.SEC == input$sub_sup_sec,]
    }
    if (input$sub_sec != "All"){
      data <- data[data$SEC == input$sub_sec,]
    }
    if (input$sub_sub_sec != "All"){
      data <- data[data$SUB.SEC == input$sub_sub_sec,]
    }
    data
  }, 
  
  #options = list (bSortClasses = TRUE)  

  options = list(fnRowCallback = I('
  function(nRow, aData,iDisplayIndex, iDisplayIndexFull) {
    // Bold cells for those >= 5 in the first column
    if (parseFloat(aData[18]) <= 3.0)
      $("td:eq(18)", nRow).css("color", "Green");
    else if (parseFloat(aData[14]) <= 6.0)
      $("td:eq(18)", nRow).css("color", "Yellow");
    else if (parseFloat(aData[14]) <= 9.0)
      $("td:eq(18)", nRow).css("color", "Blue");
    else 
      $("td:eq(18)", nRow).css("color", "Red");


  }'
  ))
  
  )

  
})

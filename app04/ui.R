# UI ----
ui <- dashboardPage(
  
  # title of dashboard 
  dashboardHeader(title = "My Dashboard"),
  
  # SIDEBAR ----
  dashboardSidebar(), # End of sidebar
  
  # MAIN BODY ----
  dashboardBody(
    
    # Create Row for map and graph
    fluidRow(
      
      # create box of width 8 for map
      box(
        title = "My Map", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 8, 
        leafletOutput("map")
      )
      
    ), # end of row with map and graph
    
    # create row for data table
    fluidRow(
      
      # create box of width 12 for data table
      box(
        title = "My Table",
        status = "primary",
        solidHeader = TRUE, 
        width = 12,
        DT::dataTableOutput('dt')
      )
    ) # end of row for data table
  ) # end of main dashboard body
) # end of ui code
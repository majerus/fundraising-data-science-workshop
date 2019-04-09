# UI ----
ui <- dashboardPage(
  
  # title of dashboard 
  dashboardHeader(title = "My Dashboard"),
  
  # SIDEBAR ----
  dashboardSidebar(), # End of sidebar
  
  # MAIN BODY ----
  dashboardBody(
    
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
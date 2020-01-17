# #libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

# Run app
shinyApp(ui, server)

# Server ----
Server = function(input, output) {
  
  # Reactive Picheo regular season ----
  pitching_full_tem <- reactive({
    req(input$season)

    df <- Hprs %>%
      select(years, 3:28) %>% 
      filter(years == distinct_years)
      
  })
  
  # Table picheo regular season ----
  
  
  output$picheo_rs <- DT::renderDataTable({
    req(input$season)
    
    DT::datatable(
      pitching_full_tem(),
      extensions = "ColReorder",
      options = list(
        pageLegth = 25,
        autoWidth = TRUE,
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:4))
      )
  
      )
  })
  
  
  
  
  
  
  
  
  
  
  
# Ends of server ----  
  
} 

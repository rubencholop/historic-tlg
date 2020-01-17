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
  pitching_full_tem = reactive({
    df <- Hprs
    
    req(inpunt$season)

    df %>%
      select(years, 3:28) %>% 
      filter(years %in% input$season)
      
  })
  
  # Table picheo regular season ----
  
  
  output$picheo_rs <- DT::renderDataTable({
    input$season
    
    DT::datatable(df %>% 
                    arrange(years))
  })
  
  
  
  
  
  
  
  
  
  
  
# Ends of server ----  
  
} 

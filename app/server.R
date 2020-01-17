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
    df <- Hprs
    req(input$season)

    df  %>%
      select(years, 3:28) %>% 
      filter(years %in% distinct_years)
      
  })
  
  # Table picheo regular season ----
  output$picheo_rs <- DT::renderDataTable({
    req(input$season)
    
    DT::datatable(
      Hprs %>% select(2:28) %>% arrange(desc(years)),
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        pageLegth = 25,
        # autoWidth = TRUE,
        # colReorder = TRUE,
        scrollX = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:4))
      )
  
      )
  })
  
  
  
  
  4801
  
  
  
  
  
  
# Ends of server ----  
  
} 

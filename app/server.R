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
      left
      # filter(years %in% list_years)
      
  })
  
  # Table picheo regular season ----
  output$picheo_rs <- DT::renderDataTable({
    req(input$season)
    
    
   df <-  pitching_full_tem() %>%
     # filter(years  %in% input$season) %>% 
     rename(
       `Temporada` = years,
       `Jugador` = jugador,
       `Edad` = edad,
       `W` = w,
       `L` = l,
       `W - L %` = 'w-l%',
       `ERA` = era,
       `G` = g,
       `GS` = gs,
       `CG` = cg,
       `SHO` = sho,
       `SV` = sv,
       `IP` = ip,
       `H` = h,
       `R` = r,
       `ER` = er,
       `HR` = hr,
       `BB` = bb,
       `SO` = so,
       `IR` = ir,
       `WHIP` = whip,
       `H/9` = 'h/9',
       `HR/9` = 'hr/9',
       `BB/9` = 'bb/9',
       `SO/9` = 'so/9',
       `SO/9` = 'so/9',
       `SO/BB` = 'so/bb',
       `BK` = 'bk') %>% 
     arrange((Temporada)) 
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        pageLegth = 50,
        lengthMenu = c(50, 75, 100),
        scrollX = TRUE,
        scrollY = "500px",
        fixedColumns = list(LeftColumns = 3 ),
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:26),
                          list(width = '100px', targets = 1))
      )
  
      )
  })
  
  
  
  

  
  
  
  
  
  
# Ends of server ----  
  
} 

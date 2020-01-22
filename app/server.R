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
    # req(input$season)

    df  %>%
      select(years, 3:28) 
      # filter(years %in% list_years)
      
  })
  
  # Table picheo regular season ----
  output$picheo_rs <- DT::renderDataTable({
    
   df <-  Hprs %>%
     select(years, 3:28) %>% 
     rename(
       `TEMPORADA` = years,
       `JUGADOR` = jugador,
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
       `SO/BB` = 'so/bb',
       `BK` = 'bk') %>% 
     arrange(TEMPORADA)
    
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
  
  
  # Table bateo regular season ----
  output$bateo_rs <- DT::renderDataTable({
    
    df <-  Hbrs %>%
      select(years, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `Edad` = edad,
        `g` = g,
        `PA` = pa,
        `AB` = ab,
        `R` = r,
        `H` = h,
        `2B` = '2b',
        `3B` = '3b',
        `HR` = hr,
        `RBI` = rbi,
        `SB` = sb,
        `CS` = cs,
        `BB` = bb,
        `SO` = so,
        `AVG` = avg,
        `OBP` = obp,
        `SLG` = slg,
        `OPS` = ops,
        `IR` = ir,
        `RC` = rc,
        `TB` = tb,
        `XB` = xb,
        `HBP` = hbp,
        `SH` = sh,
        `SF` = sf
        ) %>% 
      arrange(TEMPORADA) %>% 
      filter(
        trimws(AB) != ''  # To filter a empty value in colum AB
      )
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
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
  

  
  
  
  
  
  
  
  
  # Table picheo round robin / semi - finals ----
  output$picheo_rr_sm <- DT::renderDataTable({
    
    df <-  Hprr %>%
      select(years, refuerzo, 3:28) %>% 
      rename(
        `TEMPORADA` = years,
        `REFUERZO` = refuerzo,
        `JUGADOR` = jugador,
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
        `WHIP` = whip,
        `H/9` = 'h/9',
        `HR/9` = 'hr/9',
        `BB/9` = 'bb/9',
        `SO/9` = 'so/9',
        `SO/BB` = 'so/bb',
        `BK` = 'bk'
        ) %>% 
      arrange(TEMPORADA)
    
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
                          list(width = '100px', targets = 2))
      )
      
    )
  })
  
  
  # Table bateo regular season ----
  output$bateo_rr_sm <- DT::renderDataTable({
    
    df <-  Hbrr %>%
      select(years, refuerzo, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `Edad` = edad,
        `g` = g,
        `PA` = X5,
        `AB` = ab,
        `R` = r,
        `H` = h,
        `2B` = '2b',
        `3B` = '3b',
        `HR` = hr,
        `RBI` = rbi,
        `SB` = sb,
        `CS` = cs,
        `BB` = bb,
        `SO` = so,
        `AVG` = avg,
        `OBP` = obp,
        `SLG` = slg,
        `OPS` = ops,
        `RC` = rc,
        `TB` = tb,
        `XB` = xb,
        `HBP` = hbp,
        `SH` = sh,
        `SF` = sf
      ) %>% 
      arrange(TEMPORADA) %>% 
      filter(
        trimws(PA) != ''  # To filter a empty value in colum AB
      )
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
        pageLegth = 50,
        lengthMenu = c(50, 75, 100),
        scrollX = TRUE,
        scrollY = "500px",
        fixedColumns = list(LeftColumns = 3 ),
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:26),
                          list(width = '100px', targets = 2))
      )
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  # Table picheo finals ----
  output$picheo_finals <- DT::renderDataTable({
    
    df <-  Hpf %>%
      select(years, resultado, refuerzo, 3:29) %>% 
      rename(
        `TEMPORADA` = years,
        `RESULTADO` = resultado,
        `JUGADOR` = jugador,
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
        `WHIP` = whip,
        `H/9` = 'h/9',
        `HR/9` = 'hr/9',
        `BB/9` = 'bb/9',
        `SO/9` = 'so/9',
        `SO/BB` = 'so/bb',
        `BK` = 'bk',
        `REFUERZO` = refuerzo
      ) %>% 
      arrange(TEMPORADA)
    
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
                          list(width = '100px', targets =  3))
      )
      
    )
  })
  
  
  # Table bateo finals ----
  output$bateo_finals <- DT::renderDataTable({
    
    df <-  Hbf %>%
      select(years, resultado, refuerzo, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `RESULTADO` = resultado,
        `Edad` = edad,
        `g` = g,
        `PA` = X5,
        `AB` = ab,
        `R` = r,
        `H` = h,
        `2B` = '2b',
        `3B` = '3b',
        `HR` = hr,
        `RBI` = rbi,
        `SB` = sb,
        `CS` = cs,
        `BB` = bb,
        `SO` = so,
        `AVG` = avg,
        `OBP` = obp,
        `SLG` = slg,
        `OPS` = ops,
        `RC` = rc,
        `TB` = tb,
        `XB` = xb,
        `HBP` = hbp,
        `SH` = sh,
        `SF` = sf
      ) %>% 
      arrange(TEMPORADA) %>% 
      filter(
        trimws(PA) != ''  # To filter a empty value in colum AB
      )
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
        pageLegth = 50,
        lengthMenu = c(50, 75, 100),
        scrollX = TRUE,
        scrollY = "500px",
        fixedColumns = list(LeftColumns = 3 ),
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:26),
                          list(width = '100px', targets = 3))
      )
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Table por jugador ----
  output$info_jugador <- DT::renderDataTable({
    
    df <-  Hbf %>%
      select(years, resultado, refuerzo, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `RESULTADO` = resultado,
        `Edad` = edad,
        `g` = g,
        `PA` = X5,
        `AB` = ab,
        `R` = r,
        `H` = h,
        `2B` = '2b',
        `3B` = '3b',
        `HR` = hr,
        `RBI` = rbi,
        `SB` = sb,
        `CS` = cs,
        `BB` = bb,
        `SO` = so,
        `AVG` = avg,
        `OBP` = obp,
        `SLG` = slg,
        `OPS` = ops,
        `RC` = rc,
        `TB` = tb,
        `XB` = xb,
        `HBP` = hbp,
        `SH` = sh,
        `SF` = sf
      ) %>% 
      arrange(TEMPORADA) %>% 
      filter(
        trimws(PA) != ''  # To filter a empty value in colum AB
      ) %>% 
      select(-name)
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
        pageLegth = 50,
        lengthMenu = c(50, 75, 100),
        scrollX = TRUE,
        scrollY = "500px",
        fixedColumns = list(LeftColumns = 3 ),
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:26),
                          list(width = '100px', targets = 3))
      )
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # image jugador ----
#   output$info_jugador <- renderImage({
#     req(input$select_jugador)
# 
#     img(src = paste(input$select_jugador, '.jpg', sep = ''),
#           height = '300px')
# 
# })
  
  # User Ruben Lopez ----
  output$user <- renderUser({
        dashboardUser(
            name = "Ruben López",
            # src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
            src = "ruben.jpg",
            title = "shinydashboardPlus",
            subtitle = "Author",
           footer = p("The footer", class = "text-center"),
            fluidRow(
             dashboardUserItem(
              width = 6,
              socialButton(
               url = "https://dropbox.com",
              type = "dropbox"
              )
             ),
             dashboardUserItem(
             width = 6,
              socialButton(
              url = "https://github.com",
              type = "github"
              )
             )
            )
           )
        })
  
# Ends of server ----  
} 


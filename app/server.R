# #libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

# Run app
shinyApp(ui, server)

# Server ----
Server = function(input, output) {
 
  # Reactive info player ----
  info_player <- reactive({
    req(input$select_jugador)
    
    df <- Rosters %>% 
      filter(jugador %in% input$select_jugador) 

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
      select(years, 2:27) %>% 
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Table por  Bat_rs  by jugador ----
  output$bat_rs <- DT::renderDataTable({
    req(input$select_jugador)
    
    player_summarise <- Hbrs %>%
      filter(jugador == input$select_jugador) %>%
      select(1:27) %>%
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        pa = as.numeric(pa),
        ab = as.numeric(ab),
        r = as.numeric(r),
        h = as.numeric(h),
        hr = as.numeric(hr),
        rbi = as.numeric(rbi),
        sb = as.numeric(sb),
        cs = as.numeric(cs),
        bb = as.numeric(bb),
        so = as.numeric(so),
        avg = as.numeric(avg),
        obp = as.numeric(obp),
        slg = as.numeric(slg),
        ops = as.numeric(ops),
        ir = as.numeric(ir),
        rc = as.numeric(rc),
        tb = as.numeric(tb),
        xb = as.numeric(xb),
        hbp = as.numeric(hbp),
        sh = as.numeric(sh),
        sf = as.numeric(sf),
        `2b` = as.numeric(`2b`),
        `3b` = as.numeric(`3b`)
      ) %>%
      summarise(
        years = 'Totales',
        jugador = last(jugador),
        edad = NROW(edad),
        g = sum(g, na.rm = T),
        pa = sum(pa, na.rm = T),
        ab = sum(ab, na.rm = T),
        r = sum(r, na.rm = T),
        h = sum(h, na.rm = T),
        `2b` = sum(`2b`, na.rm = T),
        `3b` = sum(`3b`, na.rm = T),
        hr = sum(hr, na.rm = T),
        rbi = sum(rbi, na.rm = T),
        sb = sum(sb, na.rm = T),
        cs = sum(cs, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        avg = round(mean(avg, na.rm = T), 3),
        obp = round(mean(obp, na.rm = T), 3),
        slg = round(mean(slg, na.rm = T), 3),
        ops = round(mean(ops, na.rm = T), 3),
        ir = sum(ir, na.rm = T),
        rc = sum(rc, na.rm = T),
        tb = sum(tb, na.rm = T),
        xb = sum(xb, na.rm = T),
        hbp = sum(hbp, na.rm = T),
        sh = sum(sh, na.rm = T),
        sf = sum(sf, na.rm = T)
      )


    batting_player <- Hbrs %>%
      filter(jugador == input$select_jugador) %>%
      select(1:27) %>%
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        pa = as.numeric(pa),
        ab = as.numeric(ab),
        r = as.numeric(r),
        h = as.numeric(h),
        hr = as.numeric(hr),
        rbi = as.numeric(rbi),
        sb = as.numeric(sb),
        cs = as.numeric(cs),
        bb = as.numeric(bb),
        so = as.numeric(so),
        avg = as.numeric(avg),
        obp = as.numeric(obp),
        slg = as.numeric(slg),
        ops = as.numeric(ops),
        ir = as.numeric(ir),
        rc = as.numeric(rc),
        tb = as.numeric(tb),
        xb = as.numeric(xb),
        hbp = as.numeric(hbp),
        sh = as.numeric(sh),
        sf = as.numeric(sf),
        `2b` = as.numeric(`2b`),
        `3b` = as.numeric(`3b`)
      )
    
     df <- rbind(batting_player, player_summarise) %>% 
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
         `RC` = rc,
         `TB` = tb,
         `XB` = xb,
         `HBP` = hbp,
         `SH` = sh,
         `SF` = sf
       ) %>% 
       arrange(TEMPORADA) 


    # Datatable ----
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
  
  
  # Table por  Bat_rr  by jugador ----
  output$bat_rr <- DT::renderDataTable({
    req(input$select_jugador)
    
    df <-  Hbrr %>%
      select(years, refuerzo, 2:27) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `REFUERZO` = refuerzo,
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
        trimws(AB) != ''  # To filter a empty value in colum AB
      ) %>% 
      filter(JUGADOR == input$select_jugador) 
    
    
    # Datatable ----
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
  
  
  
  
  
  
  
  
  
  
  
  
  # Table por  Bat_finals  by jugador ----
  output$bat_final <- DT::renderDataTable({
    req(input$select_jugador)
    
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
      select(-name) %>% 
      filter(JUGADOR == input$select_jugador) 
    
    
    # Datatable ----
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
  
  # InfoBox Position player ----
  output$pos <- renderInfoBox({
    
    pos_player <- info_player() %>% 
      select(pos) %>%
      summarise(
        pos = last(pos))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('Posición',
            value = pos_player,
            color = 'blue',
            icon = icon('baseball', lib = 'font-awesome')
    )
  })
  
  # InfoBox lan player ----
  output$lan <- renderInfoBox({
    
    lan_player <- info_player() %>% 
      select(lan) %>%
      summarise(
        lan = last(lan))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('Lanza',
            value = lan_player,
            color = 'blue',
            icon = icon('baseball-ball', lib = 'font-awesome')
    )
  })
  
  # InfoBox bat player ----
  output$bat <- renderInfoBox({
    
    bat_player <- info_player() %>% 
      select(bat) %>%
      summarise(
        bat = last(bat))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('Batea',
            value = bat_player,
            color = 'blue',
            icon = icon('usd', lib = 'glyphicon')
    )
  })
  
  
  
  
  
  
  # InfoBox pais player ----
  output$pais <- renderInfoBox({
    
    pais_player <- info_player() %>% 
      select(pais) %>%
      summarise(
        pais = last(pais))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('País',
            value = pais_player,
            color = 'red',
            icon = icon('globe-americas', lib = 'font-awesome')
    )
  })
  
  # InfoBox estado player ----
  output$estado <- renderInfoBox({
    
    estado_player <- info_player() %>% 
      select(estado) %>%
      summarise(
        estado = last(estado))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('Estado',
            value = estado_player,
            color = 'red',
            icon = icon('map-marker', lib = 'font-awesome')
    )
  })
  
  # InfoBox ciudad player ----
  output$ciudad <- renderInfoBox({
    
    ciudad_player <- info_player() %>% 
      select(ciudad) %>%
      summarise(
        ciudad = last(ciudad))
    
    # box_color = if_else(net_profit >= 0, 'green', 'red')
    
    infoBox('Ciudad',
            value = ciudad_player,
            color = 'red',
            icon = icon('building', lib = 'font-awesome')
    )
  })

  # image jugador ----
  output$jugador_ <- renderImage({
    req(input$select_jugador)
    player <- paste('www/', input$select_jugador, '.jpg', sep = '')
    
    if (is.null(input$select_jugador))
      return(cat('Not image'))
    
    if (input$select_jugador == input$select_jugador) {
      return(list(
        src = player,
        contentType = "image/jpg"
        # width = 400,
        # height = 300
        # alt = 'Selecciona un jugador'
      ))
    }
  }, deleteFile = FALSE)
  
  # Text output Jugador
  output$jugador_bat <- renderText({
    req(input$select_jugador)
    
  paste(input$select_jugador)
  })
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


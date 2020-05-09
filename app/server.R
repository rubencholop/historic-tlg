# #libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

# Run app
  shinyApp(ui, server)

# Server ----
Server = function(input, output) {
  # Sidebar collapsible ----
  output$collapsible_sidebar = renderMenu({
    sidebarMenu(
    # br(),
    # meniItem Tiburones de la Guaira ----
    menuItem(
      text = 'Inicio',
      tabName = 'inicio',
      badgeLabel = "new", 
      badgeColor = "green",
      icon = icon('text-height', lib = 'font-awesome')
    ),
    # menuItem  Datos ----
    menuItem(
      'Datos',
      tabName = 'datos',
      icon = icon('chart-line', lib = 'font-awesome'),
      menuSubItem('Equipo', tabName = 'equipo'),
      menuSubItem('Temporada', tabName = 'temporada'),
      menuSubItem('Por Jugador', tabName = 'jugador')
    ),
    # menuItem Geo Estadisticas ----
    menuItem(
      'Geo Estadísticas',
      startExpanded = FALSE,
      tabName = 'geo_estadisticas',
      icon =  icon('globe-americas', lib = 'font-awesome'),
      menuSubItem('Geograficas', tabName = 'geo'),
      menuSubItem('Caracateristicas', tabName = 'hab')
    ),
    # menuItem Records ----
    menuItem(
      'Records',
      tabName = 'records',
      icon = icon('edit', lib = 'font-awesome'),
      menuSubItem('Historicos', tabName = 'historicos'),
      menuSubItem('Por temporadas', tabName = 'p_tem'),
      menuSubItem('Records en LVBP', tabName = 'lvbp'),
      menuSubItem('Sabermetria', tabName = 'saberm')
    ),
    # menuItem Historia ----
    menuItem(
      'Historia',
      tabName = 'historia',
      icon = icon('search-location', lib = 'font-awesome'),
      menuSubItem('En números', tabName = 'en_num'),
      menuSubItem('Estadio', tabName = 'rr_sm')
    ),
    # menuItem Glosario ----
    menuItem(
      'Glosario',
      tabName = 'glosario',
      icon = icon('google', lib = 'font-awesome'),
      menuSubItem('Glosario Sabermetrico', tabName = 'g_saberm'),
      menuSubItem('Cálculos', tabName = 'calc')
    )
  )
  
})
  
  
  
  
  # Reactive 
  
  
  # Reactive info player ----
  info_player <- reactive({
    req(input$select_jugador)
    
    df <- Rosters %>% 
      filter(jugador %in% input$select_jugador) 

  })
  
  
  # Boards  
  
  # -----TABLES ----
  # Table picheo regular season ----
  output$picheo_rs <- DT::renderDataTable({
    req(input$select_temporada)
    
    player_summarise <- Hprs %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        ir = as.numeric(ir),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Jugadores',
        jugador = NROW(jugador),
        edad = round(mean(edad, na.rm = T), 1),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = sum(ip, na.rm = T),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        ir = sum(ir, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T)
      )
    
    
    pitching_player <- Hprs %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        ir = as.numeric(ir),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk)
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        Jugador = jugador,  
        `Edad` = edad,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
       
   # Datatable ----
   headerCallback <- c(
     "function(thead, data, start, end, display){",
     "  $('th', thead).css('border-bottom', 'none');",
     "}"
   )  # To deleate header line horizontal in bottom of colums name
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      options = list(
        autoWidth = TRUE,
        dom = 'ft',  # To remove showing 1 to n of entries fields
        searching = FALSE,
        paging = FALSE,
        pageLegth = 30,
        # lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = c(0, 2:24))
                          # list(width = '100px', targets = 1)
                          ),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
          )
        )
      ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Jugadores'), "bold")
      )
    
  })
  
  # Table picheo round robin / semi - finals ----
  output$picheo_rr_sm <- DT::renderDataTable({
    
    player_summarise <- Hprr %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Jugadores',
        jugador = NROW(jugador),
        edad = round(mean(edad), 1),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = sum(ip, na.rm = T),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T),
        refuerzo = '-'
      )
    
    
    pitching_player <- Hprr %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = round(as.numeric(era), 3),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk),
        refuerzo = refuerzo
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        Jugador = jugador,
        Refuerzo = refuerzo,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name
    
    # footerCallback <- c(
    #   "function(tfoot, data, start, end, display){",
    #   "  $('th', tfoot).css('border-bottom', 'none');",
    #   "}"
    # )
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
      #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = c(0, 2:24))),
        headerCallback = JS(headerCallback),
        # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Jugadores'), "bold")
      )
    
  })
  
  
  # Table picheo finals ----
  output$picheo_finals <- DT::renderDataTable({
    req(input$select_temporada)
    
    player_summarise <- Hpf %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = round(as.numeric(era), 2),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Jugadores',
        jugador = NROW(jugador),
        edad = round(mean(edad), 1),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = round(sum(ip, na.rm = T), 1),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T),
        refuerzo = '-',
        resultado = '-'
      )
    
    
    pitching_player <- Hpf %>%
      filter(years == input$select_temporada) %>%
      select(-key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = round(as.numeric(ip), 1),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk),
        refuerzo = refuerzo,
        resultado = resultado
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        Jugador = jugador,
        Refuerzo = refuerzo,
        Resultado = resultado,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name
    
    # footerCallback <- c(
    #   "function(tfoot, data, start, end, display){",
    #   "  $('th', tfoot).css('border-bottom', 'none');",
    #   "}"
    # )
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
      #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 30,
        # lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:25)),
        headerCallback = JS(headerCallback),
        # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Jugadores'), "bold")
      )
    
  })
  
  
  # Table bateo regular season ----
  output$bateo_rs <- DT::renderDataTable({
    req(input$select_temporada_bat)
    
    player_summarise <- Hbrs %>%
      filter(years == input$select_temporada_bat,
             trimws(pa) != '' ) %>%  # To filter a empty value in colum AB
      select(-key) %>%
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
        years = 'Jugadores',
        jugador = NROW(jugador),
        edad = mean(edad),
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
      filter(years == input$select_temporada_bat) %>%
      select(-key) %>%
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
        avg = round(as.numeric(avg), 3), 
        obp = round(as.numeric(obp), 3),
        slg = round(as.numeric(slg), 3),
        ops = round(as.numeric(ops), 3),
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
      filter(
        trimws(pa) != ''  # To filter a empty value in colum AB
      )
    
    df <- rbind(batting_player, player_summarise) %>% 
      rename(
        Temporada = years,
        Jugador = jugador,
        `Edad` = edad,
        `G` = g,
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
        `IR` = ir,
        `HBP` = hbp,
        `SH` = sh,
        `SF` = sf
      ) %>% 
      arrange(Temporada)
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name
    
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        # autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 40,
        # lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = c(0, 2:26)),
                          list(width = '100px', targets = 1)),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Jugadores'), "bold")
      )
    
    
  })
  
  # Table bateo round robin / semi - finals   ----
  output$bateo_rr_sm <- DT::renderDataTable({
    
    df <-  Hbrr %>%
      select(years, refuerzo, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `EDAD` = edad,
        `REFUERZO` = refuerzo,
        `G` = g,
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
  
  
  
  
  
  
  
  
  
  
  # Table bateo finals ----
  output$bateo_finals <- DT::renderDataTable({
    
    df <-  Hbf %>%
      select(years, resultado, refuerzo, 2:28) %>% 
      rename(
        `TEMPORADA` = years,
        `JUGADOR` = jugador,
        `RESULTADO` = resultado,
        `EDAD` = edad,
        `G` = g,
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Table por Bat_rs  by jugador ----
  output$bat_rs <- DT::renderDataTable({
    req(input$select_jugador)
    
    player_summarise <- Hbrs %>%
      filter(jugador == input$select_jugador) %>%
      select(-jugador, -key) %>%
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
        years = 'Temporadas',
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
      select(-jugador, -key) %>%
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
         `Temporada` = years,
         `Edad` = edad,
         `G` = g,
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
         `IR` = ir,
         `HBP` = hbp,
         `SH` = sh,
         `SF` = sf
       ) %>% 
       arrange(Temporada) 


    # Datatable ----
     headerCallback <- c(
       "function(thead, data, start, end, display){",
       "  $('th', thead).css('border-bottom', 'none');",
       "}"
     )  # To deleate header line horizontal in bottom of colums name
     
    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        # autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:25),
                          list(width = '100px', targets = 1)),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
    
  })
  
  
  # Table por Bat_rr  by jugador ----
  output$bat_rr <- DT::renderDataTable({
    req(input$select_jugador)

    player_summarise <- Hbrr %>%
      filter(jugador == input$select_jugador) %>%
      select(2:28) %>% 
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        X5 = as.numeric(X5),
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
        years = 'Temporadas',
        jugador = last(jugador),
        edad = NROW(edad),
        g = sum(g, na.rm = T),
        X5 = sum(X5, na.rm = T),
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
        rc = sum(rc, na.rm = T),
        tb = sum(tb, na.rm = T),
        xb = sum(xb, na.rm = T),
        hbp = sum(hbp, na.rm = T),
        sh = sum(sh, na.rm = T),
        sf = sum(sf, na.rm = T),
        refuerzo = ''
      )
    
    
    batting_player <- Hbrr %>%
      filter(jugador == input$select_jugador) %>%
      select(2:28) %>% 
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        X5 = as.numeric(X5),
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
        rc = as.numeric(rc),
        tb = as.numeric(tb),
        xb = as.numeric(xb),
        hbp = as.numeric(hbp),
        sh = as.numeric(sh),
        sf = as.numeric(sf),
        `2b` = as.numeric(`2b`),
        `3b` = as.numeric(`3b`)
      )
    
    df <-  rbind(player_summarise, batting_player) %>%
      select(years, 3:27, refuerzo) %>% 
      rename(
        `Temporada` = years,
        `Refuerzo` = refuerzo,
        `Edad` = edad,
        `G` = g,
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
      arrange(Temporada)  
    
    # Datatable ----
    
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name
    
    DT::datatable(
      df,
      class = 'display', # To remove lines horizontal in table
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
        dom = 'ft',  # To remove showing 1 to n of entries fields
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:25),
                          list(width = '100px', targets = 1)),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
    
  })
  
  # Table por Bat_finals  by jugador ----
  output$bat_final <- DT::renderDataTable({
    req(input$select_jugador)
    
    player_summarise <- Hbf %>% 
      filter(jugador == input$select_jugador) %>% 
      select(years, resultado, refuerzo, 2:29) %>% 
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        X5 = as.numeric(X5),
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
        years = 'Temporadas',
        resultado = '',
        jugador = '-',
        edad = NROW(edad),
        refuerzo = '',
        g = sum(g, na.rm = T),
        X5 = sum(X5, na.rm = T),
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
        rc = sum(rc, na.rm = T),
        tb = sum(tb, na.rm = T),
        xb = sum(xb, na.rm = T),
        hbp = sum(hbp, na.rm = T),
        sh = sum(sh, na.rm = T),
        sf = sum(sf, na.rm = T)
      )
    
    
    batting_player <- Hbf %>% 
      filter(jugador == input$select_jugador) %>% 
      select(years, resultado, refuerzo, 2:29) %>% 
      mutate(
        edad = as.numeric(edad),
        g = as.numeric(g),
        X5 = as.numeric(X5),
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
        rc = as.numeric(rc),
        tb = as.numeric(tb),
        xb = as.numeric(xb),
        hbp = as.numeric(hbp),
        sh = as.numeric(sh),
        sf = as.numeric(sf),
        `2b` = as.numeric(`2b`),
        `3b` = as.numeric(`3b`)
      ) %>%
      select(-name)
    
    

    df <- rbind(batting_player, player_summarise) %>%
      select(years, 5:28, refuerzo, resultado) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        `G` = g,
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
        `SF` = sf,
        `Refuerzo` = refuerzo,
        `Logro` = resultado
      ) %>% 
      arrange(Temporada)  %>% 
      filter(
        trimws(PA) != ''  # To filter a empty value in colum AB
      ) %>% 
      mutate(Logro = if_else(Logro == 'campeon', 'Campeon', Logro),
             Logro = if_else(Logro == 'subcampeon', 'SubCampeon', Logro)
             ) %>% 
      replace(., is.na(.), 0)
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    ) # To deleate header line horizontal in bottom of colums name
    
    DT::datatable(
      df,
      class = 'display', # To remove lines horizontal in table
      extensions = "ColReorder",
      rownames = FALSE,
      options = list(
        # autoWidth = TRUE,
        dom = 'ft',  # To remove showing 1 to n of entries fields
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedColumns = list(LeftColumns = 3),
        fixedHeader = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 0:26),
                          list(width = '50px', targets = 26)
                               ),
        headerCallback = JS(headerCallback),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Rajdhani'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Rajdhani'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
    
  })
  
  
  # Infoboxes
  # Table por Pit_rs  by jugador ----
  output$picheo_jugador <- DT::renderDataTable({
    req(input$select_jugador_pit)
    
    player_summarise <- Hprs %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        ir = as.numeric(ir),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Temporadas',
        edad = NROW(edad),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = sum(ip, na.rm = T),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        ir = sum(ir, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T)
      )
    
    
    pitching_player <- Hprs %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        ir = as.numeric(ir),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk)
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name

    # footerCallback <- c(
    #   "function(tfoot, data, start, end, display){",
    #   "  $('th', tfoot).css('border-bottom', 'none');",
    #   "}"
    # )

    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
      #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:23)),
        headerCallback = JS(headerCallback),
        # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
  })
  
  
  
  
  
  # Table por Pit_rr  by jugador ----
  output$picheo_jugador_rr <- DT::renderDataTable({
    req(input$select_jugador_pit)
    
    player_summarise <- Hprr %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Temporadas',
        edad = NROW(edad),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = sum(ip, na.rm = T),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T),
        refuerzo = '-'
      )
    
    
    pitching_player <- Hprr %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk),
        refuerzo = refuerzo
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        Refuerzo = refuerzo,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name

    # footerCallback <- c(
    #   "function(tfoot, data, start, end, display){",
    #   "  $('th', tfoot).css('border-bottom', 'none');",
    #   "}"
    # )

    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
      #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:23)),
        headerCallback = JS(headerCallback),
        # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
  })
  
  
  
  
  
  # Table por Pit_finals by jugador ----
  output$picheo_jugador_final <- DT::renderDataTable({
    req(input$select_jugador_pit)
    
    player_summarise <- Hpf %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = as.numeric(whip),
        `h/9` = as.numeric(`h/9`),
        `hr/9` = as.numeric(`hr/9`),
        `bb/9` = as.numeric(`bb/9`),
        `so/9` = as.numeric(`so/9`),
        `so/bb` = as.numeric(`so/bb`),
        bk = as.numeric(bk)
      ) %>%
      summarise(
        years = 'Temporadas',
        edad = NROW(edad),
        w = sum(w, na.rm = T),
        l = sum(l, na.rm = T),
        era = round(mean(era, na.rm = T), 2),
        g = sum(g, na.rm = T),
        gs = sum(gs, na.rm = T),
        cg = sum(cg, na.rm = T),
        sho = sum(sho, na.rm = T),
        sv = sum(sv, na.rm = T),
        ip = sum(ip, na.rm = T),
        h = sum(h, na.rm = T),
        r = sum(r, na.rm = T),
        er = sum(er, na.rm = T),
        hr = sum(hr, na.rm = T),
        bb = sum(bb, na.rm = T),
        so = sum(so, na.rm = T),
        whip = round(mean(whip, na.rm = T), 2),
        `h/9` = round(mean(`h/9`, na.rm = T), 2),
        `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
        `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
        `so/9` = round(mean(`so/9`, na.rm = T), 2),
        `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
        bk = sum(bk, na.rm = T),
        refuerzo = '-',
        resultado = '-'
      )
    
    
    pitching_player <- Hpf %>%
      filter(jugador == input$select_jugador_pit) %>%
      select(-jugador, -key, -`w-l%`) %>%
      mutate(
        edad = as.numeric(edad),
        w = as.numeric(w),
        l = as.numeric(l),
        era = as.numeric(era),
        g = as.numeric(g),
        gs = as.numeric(gs),
        cg = as.numeric(cg),
        sho = as.numeric(sho),
        sv = as.numeric(sv),
        ip = as.numeric(ip),
        h = as.numeric(h),
        r = as.numeric(r),
        er = as.numeric(er),
        hr = as.numeric(hr),
        bb = as.numeric(bb),
        so = as.numeric(so),
        whip = round(as.numeric(whip), 2),
        `h/9` = round(as.numeric(`h/9`), 2), 
        `hr/9` = round(as.numeric(`hr/9`), 2),
        `bb/9` = round(as.numeric(`bb/9`), 2),
        `so/9` = round(as.numeric(`so/9`), 2),
        `so/bb` = round(as.numeric(`so/bb`), 2),
        bk = as.numeric(bk),
        refuerzo = refuerzo,
        resultado = resultado
      ) 
    
    df <- rbind(pitching_player, player_summarise) %>% 
      select(-bk) %>% 
      rename(
        `Temporada` = years,
        `Edad` = edad,
        Refuerzo = refuerzo,
        Resultado = resultado,
        `W` = w,
        `L` = l,
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
        `H/9` = `h/9`,
        `HR/9` = `hr/9`,
        `BB/9` = `bb/9`,
        `SO/9` = `so/9`,
        `SO/BB` = `so/bb`) %>% 
      arrange(Temporada) 
    
    # Datatable ----
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('border-bottom', 'none');",
      "}"
    )  # To deleate header line horizontal in bottom of colums name

    # footerCallback <- c(
    #   "function(tfoot, data, start, end, display){",
    #   "  $('th', tfoot).css('border-bottom', 'none');",
    #   "}"
    # )

    DT::datatable(
      df,
      extensions = "ColReorder",
      rownames = FALSE,
      style = ,
      # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
      #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
      options = list(
        dom = 'ft',  # To remove showing 1 to n of entries fields
        autoWidth = TRUE,
        searching = FALSE,
        paging = FALSE,
        pageLegth = 15,
        lengthMenu = c(15, 20, 25),
        lengthChange = FALSE,
        scrollX = TRUE,
        rownames = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(LeftColumns = 3),
        columnDefs = list(list(className = "dt-center", targets = 0:24)),
        headerCallback = JS(headerCallback),
        # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-family': 'Calibri'});",
          "$(this.api().table().body()).css({'font-size': '12px'});",
          "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
          "}"
        )
      )
    ) %>% 
      formatStyle(
        'Temporada',
        target = "row",
        fontWeight = styleEqual(c('Temporadas'), "bold")
      )
  })
  
  
  
  
  
  
  # ------ INFOBOX -----
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

  
  
  # Images
  
  # -----IMAGE ----
  # image Battingplayer ----
  output$jugador_ <- renderImage({
    req(input$select_jugador)
    
    player <- paste('www/batting/', input$select_jugador, '.jpg', sep = '')
    
    if (is.null(input$select_jugador))
      return(cat('Not image'))
    
    if (input$select_jugador == input$select_jugador) {
      return(list(
        src = player,
        contentType = "image/jpg",
        width = 300,
        height = 300
        # alt = 'Selecciona un jugador'
      ))
    }
  }, deleteFile = FALSE)
  
  
  # Text Outputs
  
  
  # image PitchingPlayer ----
  output$jugador_pit <- renderImage({
    req(input$select_jugador_pit)
    
    player <- paste('www/pitching/',input$select_jugador_pit, '.jpg', sep = '')
    
    if (is.null(input$select_jugador_pit))
      return(cat('Not image'))
    
    if (input$select_jugador_pit == input$select_jugador_pit) {
      return(list(
        src = player,
        contentType = "image/jpg"
        # width = 300,
        # height = 300
        # alt = 'Selecciona un jugador'
      ))
    }
  }, deleteFile = FALSE)
  
  # ----TEXT OUTPUT -----
  # Text output Jugador bat ----
  output$jugador_bat <- renderText({
    req(input$select_jugador)
    
  df <- Rosters %>% 
    filter( jugador == input$select_jugador) %>% 
    select(name) %>% 
    unique() %>% 
    pull()
  })
  # Text output Jugador position ----
  output$pos_jugador <- renderText({
    req(input$select_jugador)
    
    df <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(pos) %>% 
      summarise(
        pos = last(pos)
      ) %>% 
      unique() %>% 
      pull()
    
    paste('Posición :', df, sep = ' ')
    
  })
  
  # Text output Jugador batea-lanza ----
  output$bl_jugador <- renderText({
    req(input$select_jugador)
    
    df <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(bat) %>% 
      summarise(
        bat = last(bat)
      ) %>% 
      unique() %>% 
      pull()
    
    df1 <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(lan) %>% 
      summarise(
        lan = last(lan)
      ) %>% 
      unique() %>% 
      pull()
    
    paste('B / L :', df, '/', df1, sep = ' ')
    
    # paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    
  })
  
  # Text output Jugador pais ----
  output$pais_jugador <- renderText({
    req(input$select_jugador_pit)
    
    
    df <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(pais) %>% 
      summarise(
        pais = last(pais)
      ) %>% 
      unique() %>% 
      pull()
    
    df1 <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(estado) %>% 
      summarise(
        estado = last(estado)
      ) %>% 
      unique() %>% 
      pull()
    
    df2 <- Rosters %>% 
      filter( jugador == input$select_jugador) %>% 
      select(ciudad) %>% 
      summarise(
        ciudad = last(ciudad)
      ) %>% 
      unique() %>% 
      pull()
    
    paste(df,':', df1, '-', df2, sep = '  ')

  })
  
  # User
  
  
  
  
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



if(interactive()){
  library(shiny)
  library(bs4Dash)
  
  shiny::shinyApp(
    # Page ----
    ui = bs4DashPage(
      enable_preloader = TRUE,
      # Navbar ----
      navbar = bs4DashNavbar(
        title = tagList(
          span(class = "logo-lg", "Tiburones de la Guaira")
        ),
        # (title = span(tagList(icon("calendar"), "Example"))
        # fixed = TRUE,
        titleWidth = 233,
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "bars"
        
        # User Ruben Lopez
        # userOutput("user")
      ),
      # sidebar ----
      sidebar = bs4DashSidebar(
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
          # menuItem Estadisticas ----
          menuItem(
            'Estadísticas',
            startExpanded = FALSE,
            tabName = 'geo_estadisticas',
            icon =  icon('globe-americas', lib = 'font-awesome'),
            menuSubItem('Geograficas', tabName = 'geograficas'),
            menuSubItem('Historicas', tabName = 'istoricas')
          ),
          # menuItem Records ----
          menuItem(
            'Records',
            tabName = 'records',
            icon = icon('edit', lib = 'font-awesome'),
            menuSubItem('De por vida', tabName = 'deporvida'),
            menuSubItem('Por temporadas', tabName = 'por_temporadas'),
            menuSubItem('Records en LVBP', tabName = 'lvbp'),
            menuSubItem('Sabermetria', tabName = 'saberm')
          ),
          # menuItem Historia ----
          menuItem(
            'Historia',
            tabName = 'historia',
            icon = icon('search-location', lib = 'font-awesome'),
            menuSubItem('Tiburones de la Guaira', tabName = 'en_num'),
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
        ),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        tags$head(
          tags$style(
            HTML('
            .main-sidebar .skin-blue .left-side {
              background-color: #C20B10;
                }'
            )
          )
        )
      ),
      # control bar ----
      controlbar = bs4DashControlbar(),
      # footer ----
      footer = bs4DashFooter(
        tags$head(
          tags$style(
            HTML('
            .main-footer {
              background-color: #C20B10;
                }
           .img {
              max-width: 50%;
           }
           '
            )
          )
        ),
        img(src = 'https://tjrn.sfo2.cdn.digitaloceanspaces.com/assets/tiburones/img/site/logo_top.png'),
        span(
          style = "font-size: 1em",
          span("Created by "),
          a("Ruben Lopez", 
            href = 'https://www.linkedin.com/in/ruben-lopez-28002bb4/', 
            target = "_blank")
        )
      ),
      # Title ----
      title = 'Tiburones de la Guaira B.B.C',
      # body ----
      body = bs4DashBody()
    ),
    
    # Server ----
    server = function(input, output) {
    }
  )
}



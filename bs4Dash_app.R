
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
        disable = FALSE,
        title = "Pa Encima",
        skin = "dark",
        status = "primary",
        brandColor = 'dark',
        url = NULL,
        src = NULL,
        elevation = 4,
        opacity = 0.8,
        expand_on_hover = TRUE,
        fixed = FALSE,
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
      body = bs4DashBody(
        tabItems(
        # TabItem Inicio ----
          tabItem(
            tabName = 'inicio',
            h2('Registro Estadístico historico de Tiburones de la Guaira', align = 'center')
            ),
        # TabItem Data ----
        tabItem(
          h4('Datos historicos por equipo', align = 'center'),
          tabName = 'equipo',
          bs4TabSetPanel(
            id = "tabset",
            side = "left",
            tabPanel(
            # Picheo ----
              tabName = 'Picheo',
              fluidRow(
                column(12,
                       bs4Box(
                         width = NULL,
                         title = h4("Temporada Regular", 
                                  style = "color: #b90e13;
                                          text-transform: uppercase;
                                          font-size: 1.2em;
                                          text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           lign = 'center'),
                                DT::dataTableOutput('Preseason_team')
                         )
                       )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Box(
                         width = NULL,
                         title = h4("Round Robin", 
                                    style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('Prr_team')
                         )
                      )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Box(
                         width = NULL,
                         title = h4("Final", 
                                    style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('Pfinal_team')
                         )
                      )
                )
              ),
            # Bateo ----  
            tabPanel(
              tabName = 'Bateo',
                     fluidRow(
                       column(12,
                              bs4Box(
                                width = NULL,
                                title = h4("Temporada Regular", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('Breseason_team')
                                )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Box(
                                width = NULL,
                                title = h4("Round Robin", 
                                           style = "color: #b90e13;
                                        text-transform: uppercase;
                                        font-size: 1.2em;
                                        text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('Brr_team')
                              )
                            )
                     ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Box(
                                width = NULL,
                                title = h4("Final", 
                                           style = "color: #b90e13;
                                        text-transform: uppercase;
                                        font-size: 1.2em;
                                        text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('Bfinal_team')
                              )
                           )
                        )
                      )
            )
          ),
        # TabItem by season ----
        tabItem(
          h2('Datos historicos por temporada', align = 'center'),
          tabName = 'temporada',
          tabsetPanel(
            id = "tabset1",
            side = "left",
            # TabPanel Picheo ----
            tabPanel(
              tabName = 'Picheo', 
                     # Input ----
                     fluidRow(
                       br(),
                       column(3,
                              selectInput(
                                inputId = 'select_temporada',
                                label = 'Seleccione una temporada',
                                choices = temporadas
                              )
                       )
                     ),
                     # Tables Picheo ----
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Box(
                                width = NULL,
                                title = h4("Temporada Regular", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('picheo_rs')
                              )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Box(
                                width = NULL,
                                title = h4("Round Robin", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('picheo_rr_sm')
                              )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Box(
                                width = NULL,
                                title = h4("Final", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('picheo_finals')
                              )
                              )
                       )
                       ),
            # TabPanel Bateo ----
            tabPanel(
              tabName = 'Bateo',
                     # Input ----
                     fluidRow(
                       br(),
                       column(3,
                              selectInput(
                                inputId = 'select_temporada_bat',
                                label = 'Seleccione una temporada',
                                choices = temporadas
                              )
                       )
                     ),
                     # Tables Bateo ----
                     fluidRow(
                       column(12,
                              bs4Box(
                                width = NULL,
                                title = h4("Temporada Regular", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                           align = 'center'),
                                DT::dataTableOutput('bateo_rs')
                              )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Box(
                                width = NULL,
                                title = h4("Raound Robin", 
                                           style = "color: #b90e13;
                                           text-transform: uppercase;
                                           font-size: 1.2em;
                                           text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                         align = 'center'),
                              DT::dataTableOutput('bateo_rr_sm')
                            )
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,
                            bs4Box(
                              width = NULL,
                              title = h4("Final", 
                                         style = "color: #b90e13;
                                         text-transform: uppercase;
                                         font-size: 1.2em;
                                         text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                         align = 'center'),
                              DT::dataTableOutput('bateo_finals')
                                )
                         )
                       )
              )
              # tabPanel Fildeo ----
              # tabPanel('Fildeo', tableOutput('fildeo_rs'))
            )
          )
        )
        # End body ----
      )
    ),
    
    # Server ----
    server = function(input, output) {
    }
  )




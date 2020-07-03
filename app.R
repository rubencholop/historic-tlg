# Libraries ----
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(lubridate)
  library(plotly)
  library(dplyr)
  library(DT)
  library(stringr)
  library(readr)

# Choices ----

Rosters <- read_csv('data/rosters_clean.csv')

.rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  distinct(name) %>% 
  mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
  )

Rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  left_join(.rosters, by = 'name')

# unique roster
Unique_Rosters <- Rosters %>% 
  group_by(ID) %>% 
  summarize(
    jugador = last(jugador),
    name = last(name),
    pos = last(pos),
    bat = last(bat),
    lan = last(lan),
    exp = last(exp),
    pais = last(pais),
    estado = last(estado),
    ciudad = last(ciudad)
  ) %>% 
  arrange(jugador)

.pitchers <- Rosters %>% 
  filter(pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = last(jugador)) %>% 
  arrange(jugador) %>% 
  select(jugador)

.bateadores <- Rosters %>% 
  filter(!pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = last(jugador)) %>% 
  arrange(jugador) %>% 
  select(jugador)

posiciones <- Rosters %>% 
  select(pos) %>% 
  mutate(pos = if_else(pos == "p", "P", pos)) %>% 
  unique() %>% 
  pull()



.st <- as.Date("2020-01-22")
.en <- as.Date(today())
.dates <- seq(.en, .st, by = "-1 day")
year(today()) -1


from <- 1962
to <- lubridate::year(Sys.Date()) 
range_ <- c(from:to)
pages <- c(1:(to - (from)))

season <-  function(x){
  df <- paste(range_[x], "-", substring(range_[x + 1], 3), sep = "")
  data.frame(df)
}

temporadas <- data.table::rbindlist(
  lapply(pages, season), fill = TRUE
) %>% 
  arrange(desc(df)) %>% 
  rename(temporadas = df) %>% 
  pull()

# Functions ----

IP <- function(x){
  x <- x %>%
    sum() %>% 
    as.character()
  
  episodio <-  as.numeric(sub("\\..*", "", x))
  tercio <-  as.numeric(str_sub(x, -1, -1))
  
  x <-  episodio + trunc(tercio / 3) + (tercio %% 3) / 10
  
  return(x)
}

  
  shiny::shinyApp(
    # Page ----
    ui = bs4DashPage(
      enable_preloader = TRUE,
      # Navbar ----
      navbar = bs4DashNavbar(
        tags$head(
          tags$style(
            HTML('
            .main-sidebar{
              background-color: #0473cb;
            }

            .brand-link{
            border bottom; none;
            }

            .bg-dark{
            background-color: #0473cb;
            }

           ')
          )
        ),
        div(id = "tiburones",
            h2("Registro historico de Tiburones de la Guaira", 
               style = "color:red", align = 'center'),
                # HTML(
                #     "<span style='color:#8bd5d2;
                #       font-size:30px;
                #       font-family:Segoe Script;
                #       font-style: italic;'> Pa Encima!<span>"
                # ),
            tags$style(HTML('#tiburones {align-left: auto;}'))
        ),
        # title = tagList(
        #   span(class = "logo-lg", "Tiburones de la Guaira")
        # ),
        # (title = span(tagList(icon("calendar"), "Example"))
        # fixed = TRUE,
        titleWidth = 250,
        enable_rightsidebar = FALSE,
        rightSidebarIcon = "bars"
        
        # User Ruben Lopez
        # userOutput("user")
      ),
      # Sidebar ----
      sidebar = bs4DashSidebar(
        tags$head(tags$style(HTML(
          "[class*='sidebar-dark'] .brand-link {
            border-bottom: none; 
           }
          
          [class*='sidebar-dark-'] .sidebar a {
            color: #FFFF
            }
          "
        ))),
        disable = FALSE,
        title = h4("TibuStats",
                   style = "color: #FFFF;
                            text-transform: uppercase;
                            font-size: 1.2em;
                            text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                   align = 'center'),
        skin = "dark",
        status = "primary",
        brandColor = '#011C51',
        url = NULL,
        src = "logo_top.png",
        elevation = 4,
        opacity = 0.8,
        expand_on_hover = TRUE,
        fixed = FALSE,
        sidebarMenu(
          # br(),
          # meniItem Tiburones de la Guaira ----
          bs4SidebarMenu(
            bs4SidebarMenuItem(
            text = 'Inicio',
            tabName = 'inicio',
            badgeLabel = "new", 
            badgeColor = "green",
            icon = "home",
            startExpanded = FALSE
          ),
          # MenuSubItem Datos ----
          bs4SidebarMenuItem(
            text = 'Datos',
            tabName = 'datos',
            icon = "database",
            startExpanded = FALSE,
            bs4SidebarMenuSubItem(text = 'Equipo', tabName = 'equipo', icon = "circle"),
            bs4SidebarMenuSubItem(text = 'Temporada', tabName = 'temporada', icon = "circle"),
            bs4SidebarMenuSubItem(text = 'Jugador', tabName = 'jugador', icon = "circle"),
            bs4SidebarMenuSubItem(text = 'Posicion', tabName = 'posicion', icon = "circle"),
            bs4SidebarMenuSubItem(text = 'Roster', tabName = 'roster', icon = "circle")
            )
          ),
          # menuItem Estadisticas ----
          bs4SidebarMenuItem(
            text = HTML(paste('Estadísticas', 
                          bs4Badge("New", position = "left", status = "success"))
                        ),
            startExpanded = FALSE,
            tabName = 'geo_estadisticas',
            icon = "tasks",
            bs4SidebarMenuSubItem('Geograficas', tabName = 'geograficas', icon = "circle"),
            bs4SidebarMenuSubItem('Historicas', tabName = 'istoricas', icon = "circle")
          ),
          # menuItem Records ----
          bs4SidebarMenuItem(
            text = 'Records',
            tabName = 'records',
            icon = "medal",
            bs4SidebarMenuSubItem('De por vida', tabName = 'deporvida', icon = "circle"),
            bs4SidebarMenuSubItem('Por temporadas', tabName = 'por_temporadas', icon = "circle"),
            bs4SidebarMenuSubItem('LVBP', tabName = 'lvbp', icon = "circle"),
            bs4SidebarMenuSubItem('Sabermetria', tabName = 'saberm', icon = "circle")
          ),
          # menuItem Historia ----
          bs4SidebarMenuItem(
            text ='Historia',
            tabName = 'historia',
            icon = "hourglass",
            bs4SidebarMenuSubItem('Tiburones de la Guaira', tabName = 'en_num', icon = "circle"),
            bs4SidebarMenuSubItem('Estadio', tabName = 'rr_sm', icon = "circle")
          ),
          # menuItem Glosario ----
          bs4SidebarMenuItem(
            text = 'Glosario',
            tabName = 'glosario',
            icon = "book",
            bs4SidebarMenuSubItem('Glosario Sabermetrico', tabName = 'g_saberm', icon = "circle"),
            bs4SidebarMenuSubItem('Cálculos', tabName = 'calc', icon = "circle")
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
      # Control bar ----
      controlbar = bs4DashControlbar(),
      # Footer ----
      footer = bs4DashFooter(
        tags$head(
          tags$style(
            HTML('
            .main-footer {
              background-color: #f4f6f9;
              border-top: none;
                }
           .img {
              max-width: 15%;
              vertical-align: bottom;
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
        ),
        copyrights = a(
          href = "https://twitter.com/divadnojnarg", 
          target = "_blank", "@DivadNojnarg"
        ),
        right_text = "2018"
      ),
      # Title ----
      title = 'Tiburones de la Guaira B.B.C',
      # Body ----
      body = bs4DashBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        tags$head(tags$style(HTML(
          'thead, th {
          text-align: center;
          color: #FFF;
          }
          
         /* Jistify center title of table
          .justify-content-between {
          justify-content: center;
          }
         /*
          
          '
        ))),
        tabItems(
        # TabItem Home ----
          tabItem(
            tabName = 'inicio'
            # h2('Registro Estadístico historico de Tiburones de la Guaira', align = 'center')
            ),
        # TabItem by Team ----
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
        # TabItem by Season ----
        tabItem(
          h4('Datos historicos por temporada', align = "left"),
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
                                label = 'Temporadas',
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
                                label = 'Temporadas',
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
            # TabPanel Fildeo ----
              # tabPanel(tabName = 'Fildeo', tableOutput('fildeo_rs'))
            )
          ),
        # TabItem by Player ----
        tabItem(
          h4('Datos historicos por jugador', align = 'center'),
          tabName = 'jugador',
          tabsetPanel(
            id = "tabset2",
            side = "left",
            # Picheo ----
            tabPanel(
              tabName = 'Picheo',
              # Input ----
              fluidRow(
                br(),
                column(3,
                       selectInput(
                         inputId = 'select_jugador_pit',
                         label = 'Lanzadores',
                         choices = .pitchers
                       )
                )
              ),
              # Image ----
              fluidRow(
                column(6,
                       shinydashboardPlus::widgetUserBox(
                         title = "Elizabeth Pierce",
                         subtitle = "Web Designer",
                         type = NULL,
                         width = 12,
                         src = imageOutput('jugador_pit'),
                         background = TRUE,
                         backgroundUrl = "tibu.jpeg",
                         closable = FALSE,
                         "Some text here!",
                         footer = "The footer here!"
                       )
                )
              ),
              # Table  ----
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
                         DT::dataTableOutput('picheo_jugador')
                       ),
                       br(),
                       bs4Box(
                         width = NULL,
                         title = h4("Round Robin", 
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         DT::dataTableOutput('picheo_jugador_rr')
                       ),
                       br(),
                       bs4Box(
                         width = NULL,
                         title = h4("Finales",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         DT::dataTableOutput('picheo_jugador_final')
                       ) 
                    )
                  )
                ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              # Input ----
              fluidRow(
                br(),
                column(3,
                       selectInput(
                         inputId = 'select_jugador',
                         label = "Bateadores",
                         choices = .bateadores
                       )
                )
              ),
              # Image ----
              fluidRow(
                column(7,
                       bs4Box(
                         width = NULL,
                         collapsible = TRUE,
                         title = h3(textOutput('jugador_bat'), align = 'center'),
                         column(8,
                                fluidRow(
                                  column(12,
                                         imageOutput('jugador_')
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  column(12,
                                         h3(textOutput('pos_jugador'),  align = 'rigth')
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         h3(textOutput('bl_jugador'), align = 'rigth')
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         h3(textOutput('pais_jugador'),  align = 'rigth')
                                  )
                                )
                         )
                       )
                ),
                column(5,
                       bs4Box(
                         width = NULL,
                         collapsible = TRUE,
                         # status = 'warning',
                         title = 'Radarchart'
                       )
                )
              ),
              br(),
              # Table  ----
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
                         DT::dataTableOutput('bat_rs')
                       ),
                       br(),
                       bs4Box(
                         width = NULL,
                         title = h4("Round Robin", 
                                    style = "color: #b90e13;
                                          text-transform: uppercase;
                                          font-size: 1.2em;
                                          text-shadow:1px 1px 2px rgba(150, 150, 150, 1);", 
                                    align = 'center'),
                         # status = 'warning',
                         DT::dataTableOutput('bat_rr')
                       ),
                       br(),
                       bs4Box(
                         width = NULL,
                         title = h4("Finales", 
                                    style = "color: #b90e13;
                                          text-transform: uppercase;
                                          font-size: 1.2em;
                                          text-shadow:1px 1px 2px rgba(150, 150, 150, 1);", 
                                    align = 'center'),
                         DT::dataTableOutput('bat_final')
                       ) 
                    )
                  )
              )
            )
          ),
        # TabItem by Position ----
        bs4TabItem(
          h4('Datos historicos por posicion', align = 'center'),
          tabName = 'posicion',
          # input position ----
          fluidRow(
            br(),
            column(3,
                   selectInput(
                     inputId = 'select_posicion',
                     label = 'Posiciones',
                     choices = posiciones
                   )
            )
          ),
          # Stats by position ----
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
                       DT::dataTableOutput('info_position')
                     )
                  )
              )
        ),
        # TabItem by Roster ----
        bs4TabItem(
          h4('Rosters historicos por posicion', align = 'center'),
          tabName = 'roster',
          # input roster ----
          fluidRow(
            br(),
            column(3,
                   selectInput(
                     inputId = 'select_rosters',
                     label = 'Temporadas',
                     choices = temporadas
                   )
            )
          ),
          # Stats by roster ----
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
                     DT::dataTableOutput('info_roster')
                   )
            )
          )
        ),
        # Tabitem Estadísticas ----
        tabItem(
          tabName = '',
          tabsetPanel(
            id = "tabpanel5",
            side = "left",
            tabPanel(
              tabName = 'Picheo'
            ),
            tabPanel(
              tabName  = 'Bateo'
            )
          )
        ),
        # Tabitem Records ----
        tabItem(
          tabName = 'deporvida',
          h4('Lideres historicos', align = 'center'),
          tabsetPanel(
            id = "tabpanel4",
            side = "left",
            tabPanel(
              # Picheo ----
              tabName = 'Picheo',
              #1 ----
              fluidRow(
                column(4,
                       bs4Dash::bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("W",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_w')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Dash::bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("L",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_l')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Dash::bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("G",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_g')
                                  )
                                )
                           )
                         )
                       )
              ),
              #2 ----
              fluidRow(
                column(4,
                       bs4Dash::bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("GS",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_gs')
                                  )
                                )
                         )
                       )
                ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("IP",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_ip')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("SO",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_so')
                                  )
                                )
                           )
                         )
                       )
              ),
              #3 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("H",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_h')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("BB",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_bb')
                                  )
                                )
                         )
                       )
                ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("ERA",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_era')
                                  )
                                )
                           )
                         )
                       )
              ),
              #4 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("WHIP",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_whip')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("SV",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_sv')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("IR",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_ir')
                                  )
                                )
                         )
                       )
                )
              ),
              #5 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("H/9",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_h9')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("SO/9",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_so9')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("BB/9",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_bb9')
                                  )
                                )
                           )
                         )
                       )
                ),
              #6 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("SO/BB",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('p_sobb')
                                  )
                                )
                         )
                       )
                  )
                )
              ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              #1 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("H",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_hits')
                                  )
                                )
                              )
                          )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("2B",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_2b')
                                  )
                                )
                            )
                          )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("3B",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_3b')
                                  )
                                )
                              )
                          )
                       )
              ),
              #2 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("HR",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_hr')
                                  )
                                )
                         )
                       )
                ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("AVG",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_average')
                                  )
                                )
                            )
                          )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("OBP",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center')
                          )
                       )
                  ),
              #3 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("SLG",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center')
                       )
                ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("OPS",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center')
                       )
                ),
                column(4,
                      bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("RBI",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_rbi')
                                  )
                                )
                          )
                        )
                       )
                  ),
              #4 ----
              fluidRow(
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = h2("AB",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_ab')
                                  )
                                )
                           )
                         )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("SB",
                                    style = "color: #b90e13;
                                    text-transform: uppercase;
                                    font-size: 1.2em;
                                    text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_sb')
                                  )
                                )
                             )
                           )
                       ),
                column(4,
                       bs4Box(
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = h2("XB",
                                    style = "color: #b90e13;
                                  text-transform: uppercase;
                                  font-size: 1.2em;
                                  text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                                    align = 'center'),
                         column(12,
                                fluidRow(
                                  column(12,
                                         DT::dataTableOutput('b_xb')
                                  )
                                )
                           )
                         )
                      )
                    )
                )
            )
          )
        )
        # End body ----
      )
    ),
    
    # Server ----
    server = function(input, output) {
      # Reactive Rosters ----
      Rosters <- reactive({
        .Rosters <- read_csv('data/rosters_clean.csv')
        
        .rosters <- .Rosters %>%
          arrange(jugador, years) %>%
          distinct(name) %>%
          mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
          )
        
        Rosters <- .Rosters %>%
          arrange(jugador, years) %>%
          left_join(.rosters, by = 'name')
        
      })
      
      # Reactive Batting regular season ----
      brs <- reactive({
        brs <- read_csv('data/batting_reseason.csv')
      })
      
      # Reactive Batting round robin ----
      brr <- reactive({
        brr <- read_csv('data/batting_rr.csv')
      })
      
      # Reactive Batting finals ----
      bf <- reactive({
        bf <- read_csv('data/batting_finals.csv')
      })
      
      # Reactive Pitching regular season ----
      prs <- reactive({
        prs <- read_csv('data/pitching_reseason.csv')
      })
      
      # Reactive Pitching round robin ----
      prr <- reactive({
        prr <- read_csv('data/pitching_rrobin.csv')
      })
      
      # Reactive Pitching final ----
      pf <- reactive({
        pf <- read_csv('data/pitching_finals.csv')
      })
      
      # Reactive info player ----
      info_player <- reactive({
        req(input$select_jugador)
        
        df <- Rosters %>% 
          filter(jugador %in% input$select_jugador) 
        
      })
      
      # -----TABLES ----
      #By Team
      # Table picheo regular season by team ----
      output$Preseason_team <- DT::renderDataTable({
        
        player_summarise <- prs() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad, na.rm = T), 1),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2))
            ) 
        
        pitching_player <- prs() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          group_by(years) %>% 
          summarise(
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2))
          ) %>% 
          ungroup()
        
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            # `GP` = gp,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            IR = ir,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
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
            columnDefs = list(list(className = "dt-center", targets = c(0:23))
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      # Table picheo round robin by team ----
      output$Prr_team <- DT::renderDataTable({
        
        player_summarise <- prr() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          summarise(
            years = 'Total',
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
          )
        
        pitching_player <- prr() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          group_by(years) %>% 
          summarise(
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
          )
        
        df <- rbind(pitching_player, player_summarise) %>% 
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
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`,
            `REFUERZO` = `refuerzo`
          ) %>% 
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
            columnDefs = list(list(className = "dt-center", targets = c(0:23))),
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table picheo final by team ----
      output$Pfinal_team <- DT::renderDataTable({
        
        player_summarise <- pf() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = sum(ifelse(resultado == 'campeon', 1, 0))
          )
        
        
        pitching_player <- pf() %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado)
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            REFUERZO = refuerzo,
            RESULTADO = resultado,
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
            columnDefs = list(list(className = "dt-center", targets = c(0:24))),
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table bateo regular season by team ----
      output$Breseason_team <- DT::renderDataTable({
        
        player_summarise <- brs() %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
          ) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 2),
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
        
        
        batting_player <- brs() %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
          ) %>% 
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
          )
        
        df <- rbind(batting_player, player_summarise) %>% 
          rename(
            Temporada = years,
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
            columnDefs = list(list(className = "dt-center", targets = 0:25)),
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      # Table bateo round robin by team ----
      output$Brr_team <- DT::renderDataTable({
        
        player_summarise <- brr() %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
          ) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 1),
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
            refuerzo = sum(refuerzo, na.rm = T)
          )
        
        
        batting_player <- brr() %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
          )
        
        df <-  rbind(player_summarise, batting_player) %>%
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
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 0:25)),
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table bateo final by team ----
      output$Bfinal_team <- DT::renderDataTable({
        
        player_summarise <-  bf()%>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado)
          ) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 1),
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
            refuerzo = sum(refuerzo, na.rm = T),
            resultado = sum(ifelse(resultado == 'Campeon', 1, 0))
          )
        
        
        batting_player <-  bf() %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
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
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado)
          )
        
        df <- rbind(batting_player, player_summarise) %>%
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
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedColumns = list(LeftColumns = 3),
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 0:26),
                              list(width = '80px', targets = 26)
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      #By season -----
      # Table picheo regular season ----
      output$picheo_rs <- DT::renderDataTable({
        req(input$select_temporada)
        
        player_summarise <- prs() %>% 
          filter(years == input$select_temporada) %>% 
          select(-`w-l%`, -bk) %>%
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
            `so/bb` = as.numeric(`so/bb`)
          ) %>%
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad, na.rm = T), 1),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
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
            whip = round(mean(whip, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
            `h/9` = round(mean(`h/9`, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
            `hr/9` = round(mean(`hr/9`, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
            `bb/9` = round(mean(`bb/9`, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
            `so/9` = round(mean(`so/9`, na.rm = T), 2), # Debe calsularse y no el promedio IMPORTANTE
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2) # Debe calsularse y no el promedio IMPORTANTE
          )
        
        
        pitching_player <- prs() %>%
          filter(years == input$select_temporada) %>%
          select(-`w-l%`, -bk) %>%
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
            `so/bb` = round(as.numeric(`so/bb`), 2)
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
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
            columnDefs = list(list(className = "dt-center", targets = c(0:24))
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
      
      # Table picheo round robin ----
      output$picheo_rr_sm <- DT::renderDataTable({
        req(input$select_temporada)
        
        player_summarise <- prr() %>%
          filter(years == input$select_temporada) %>%
          select(-bk, -`w-l%`) %>%
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
            `so/bb` = as.numeric(`so/bb`)
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
            refuerzo = '-'
          )
        
        
        pitching_player <- prr() %>%
          filter(years == input$select_temporada) %>%
          select(-bk, -`w-l%`) %>%
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
            refuerzo = refuerzo
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
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
            columnDefs = list(list(className = "dt-center", targets = c(0:24))),
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
      
      
      # Table picheo final ----
      output$picheo_finals <- DT::renderDataTable({
        req(input$select_temporada)
        
        player_summarise <- pf() %>%
          filter(years == input$select_temporada) %>%
          select(-bk, -`w-l%`) %>%
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
            `so/bb` = as.numeric(`so/bb`)
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
            refuerzo = '-',
            resultado = '-'
          )
        
        
        pitching_player <- pf() %>%
          filter(years == input$select_temporada) %>%
          select(-bk, -`w-l%`) %>%
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
            refuerzo = refuerzo,
            resultado = resultado
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
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
      
      
      # Table bateo regular season  ----
      output$bateo_rs <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        player_summarise <- brs() %>%
          filter(years == input$select_temporada_bat) %>% 
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
            edad = round(mean(edad), 1),
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
        
        
        batting_player <- brs() %>%
          filter(years == input$select_temporada_bat) %>%
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
            columnDefs = list(list(className = "dt-center", targets = c(0:26))),
            # list(width = '100px', targets = 1)),
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
      
      # Table bateo round robin  ----
      output$bateo_rr_sm <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        player_summarise <- brr() %>%
          filter(years == input$select_temporada_bat,
                 trimws(X5) != '' 
          ) %>%
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
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad), 1),
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
            avg = round(mean(avg, na.rm = T), 3), # Importante debe calcularse y no em promedio
            obp = round(mean(obp, na.rm = T), 3), # Importante debe calcularse y no em promedio
            slg = round(mean(slg, na.rm = T), 3), # Importante debe calcularse y no em promedio
            ops = round(mean(ops, na.rm = T), 3), # Importante debe calcularse y no em promedio
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = ''
          )
        
        
        batting_player <- brr() %>%
          filter(years == input$select_temporada_bat,
                 trimws(X5) != '' ) %>%
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
          rename(
            `Temporada` = years,
            `Refuerzo` = refuerzo,
            Jugador = jugador,
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
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:26))),
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
      
      
      # Table bateo final ----
      output$bateo_finals <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        player_summarise <- bf() %>% 
          filter(years == input$select_temporada_bat,
                 trimws(X5) != '' 
          ) %>% 
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
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad), 1),
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
            refuerzo = '',
            resultado = ''
          )
        
        
        batting_player <- bf() %>% 
          filter(years == input$select_temporada_bat,
                 trimws(X5) != '' ) %>% 
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
        
        
        
        df <- rbind(batting_player, player_summarise) %>%
          rename(
            `Temporada` = years,
            Jugador = jugador,
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
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedColumns = list(LeftColumns = 3),
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:26))),
            
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
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      
      
      #By player
      
      
      #By player
      #By player
      #By Player
      #By player ----
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
        
        player_summarise <- prs() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
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
            years = 'Total',
            edad = NROW(edad),
            edad = as.numeric(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T)
          )
        
        
        pitching_player <- prs() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>%
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      
      
      
      
      # Table por Pit_rr  by jugador ----
      output$picheo_jugador_rr <- DT::renderDataTable({
        req(input$select_jugador_pit)
        
        player_summarise <- prr() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>%
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
            years = 'Total',
            edad = NROW(edad),
            edad = as.numeric(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T),
            refuerzo = '-'
          )
        
        
        pitching_player <- prr() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>%
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      
      
      
      
      # Table por Pit_finals by jugador ----
      output$picheo_jugador_final <- DT::renderDataTable({
        req(input$select_jugador_pit)
        
        player_summarise <- pf() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>%
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
            years = 'Total',
            edad = NROW(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T),
            refuerzo = '-',
            resultado = '-'
          )
        
        
        pitching_player <- pf() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>%
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
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      #By position ----
      # Table bateo regular season by position ----
      output$info_position <- DT::renderDataTable({
        req(input$select_posicion)

        batting_player <- brs() %>%
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>% 
                      mutate(key = paste(as.character(years), jugador)) %>% 
                      select(key, pos, first_name, last_name), by = "key"
                      ) %>% 
          select(2:3, pos,first_name, last_name, 3:31) %>% 
          arrange(years, jugador)  %>% 
          filter(pos == input$select_posicion) %>% 
          select(-years, -edad) %>% 
          group_by(jugador)  %>% 
          summarise(
            pos  = last(pos),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          mutate(
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
          select(-jugador) %>% 
          mutate(jugador = paste0(first_name, " ", last_name, sep = " ")) %>% 
          select(jugador, 4:28) %>% 
          arrange(desc(g)) %>% 
          rename(
            Jugador = jugador,
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
          ) 
        
        # Datatable ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          batting_player,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:24))),
                # className = "left", targets = c(1:24))),
                                   # width = "200px", targets = 1)),
                              # list(width = '200px', targets = "_all")),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )  

      })
      
      
      #By Rosters ----
      # Table by roster ----
      output$info_roster <- DT::renderDataTable({
        req(input$select_rosters)
        
        roster <- Rosters() %>%
          filter(years == input$select_rosters) %>% 
          mutate(jugador = paste0(first_name, " ", last_name)) %>% 
          select(jugador, pos, bat, lan, pais, estado, ciudad) %>% 
          rename(
            Jugador = jugador,
            `Posicion` = pos,
            `Batea` = bat,
            `Lanza` = lan,
            Pais = pais,
            `Estado` = estado,
            `Ciudad` = ciudad
            ) %>% 
          arrange(Jugador)
        
        # Datatable ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          roster,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                # width = '120px', targets = 0,
                # width = '10px', targets = c(1:6),
                className = "dt-center", targets = c(0:6))),
            # width = "200px", targets = 1)),
            # list(width = '200px', targets = "_all")),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )  
        
      })
      
      
      #Batting Records ----
      # Table bateo lideres H ----
      output$b_hits <- renderDataTable({
        
        hits <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(h)) %>%
          select(first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, h) %>% 
          rename(
            Jugador = jugador,
            H = h
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hits,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres 2B ----
      output$b_2b <- renderDataTable({
        
        dobles <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(`2b`)) %>%
          select(first_name, last_name, `2b`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, `2b`) %>% 
          rename(
            Jugador = jugador,
            `2B` = `2b`
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          dobles,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres 3B ----
      output$b_3b <- renderDataTable({
        
        triples <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(`3b`)) %>%
          select(first_name, last_name, `3b`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, `3b`) %>% 
          rename(
            Jugador = jugador,
            `3B` = `3b`
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          triples,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres HR ----
      output$b_hr <- renderDataTable({
        
        hr <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(hr)) %>%
          select(first_name, last_name, hr) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, hr) %>% 
          rename(
            Jugador = jugador,
            HR = hr
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hr,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres AVG ----
      output$b_average <- renderDataTable({
        
        avg <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          filter(ab >= 2000) %>% 
          arrange(desc(avg)) %>% 
          select(first_name, last_name, h, ab, avg) %>% 
          mutate(avg = round(((h)/ ab), 3)) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, avg) %>% 
          select(jugador, avg) %>% 
          rename(
            Jugador = jugador,
            AVG = avg
          ) %>% 
          arrange(desc(AVG)) %>% 
          mutate(Order = seq(1, NROW(Jugador), 1)) %>% 
          select(Jugador, AVG) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          avg,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 2000 BA')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres OBP ----
      output$b_obp <- renderDataTable({
        
        obp <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          filter(ab >= 2000) %>% 
          arrange(desc(avg)) %>% 
          select(first_name, last_name, h, ab, avg) %>% 
          mutate(avg = round(((h)/ ab), 3)) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, avg) %>% 
          select(jugador, avg) %>% 
          rename(
            Jugador = jugador,
            AVG = avg
          ) %>% 
          arrange(desc(AVG)) %>% 
          mutate(Order = seq(1, NROW(Jugador), 1)) %>% 
          select(Jugador, AVG) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          avg,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 2000 BA')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres RBI ----
      output$b_rbi <- renderDataTable({
        
        rbi <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(rbi)) %>%
          select(first_name, last_name, rbi) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, rbi) %>% 
          rename(
            Jugador = jugador,
            RBI = rbi
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          rbi,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres AB ----
      output$b_ab <- renderDataTable({
        
        ab <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(ab)) %>%
          select(first_name, last_name, ab) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, ab) %>% 
          rename(
            Jugador = jugador,
            AB = ab
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ab,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres SB ----
      output$b_sb <- renderDataTable({
        
        sb <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(sb)) %>%
          select(first_name, last_name, sb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, sb) %>% 
          rename(
            Jugador = jugador,
            SB = sb
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table bateo lideres XB ----
      output$b_xb <- renderDataTable({
        
        xb <- brs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
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
          ) %>% 
          arrange(desc(xb)) %>%
          select(first_name, last_name, xb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, xb) %>% 
          rename(
            Jugador = jugador,
            XB = xb
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          xb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Pitching Record ----
      # Table picheo lideres w ----
      output$p_w <- renderDataTable({
        
        w <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(w)) %>%
          select(first_name, last_name, w) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, w) %>% 
          rename(
            Jugador = jugador,
            W = w
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          w,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres l ----
      output$p_l <- renderDataTable({
        
        l <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(w)) %>%
          select(first_name, last_name, l) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, l) %>% 
          rename(
            Jugador = jugador,
            L = l
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          l,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres g ----
      output$p_g <- renderDataTable({
        
        g <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(g)) %>%
          select(first_name, last_name, g) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, g) %>% 
          rename(
            Jugador = jugador,
            G = g
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          g,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres gs ----
      output$p_gs <- renderDataTable({
        
        gs <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(gs)) %>%
          select(first_name, last_name, gs) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, gs) %>% 
          rename(
            Jugador = jugador,
            GS = gs
          ) %>% 
          slice(1:(n()-1))
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          gs,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres ip ----
      output$p_ip <- renderDataTable({
        IP <- function(x){
          x <- x %>%
            sum() %>% 
            as.character()
          
          episodio <-  as.numeric(sub("\\..*", "", x))
          tercio <-  as.numeric(str_sub(x, -1, -1))
          
          x <-  episodio + trunc(tercio / 3) + (tercio %% 3) / 10
          
          return(x)
        }
        
        ip <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            # ip = sum(ip, na.rm = T),
            ip = IP(ip),
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
          ) %>% 
          arrange(desc(ip)) %>%
          select(first_name, last_name, ip) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, ip) %>% 
          rename(
            Jugador = jugador,
            IP = ip
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = "100px")),
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
        ) 
      })
      
      # Table picheo lideres so ----
      output$p_so <- renderDataTable({
        
        so <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(so)) %>%
          select(first_name, last_name, so) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, so) %>% 
          rename(
            Jugador = jugador,
            SO = so
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres h ----
      output$p_h <- renderDataTable({
        
        h <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(h)) %>%
          select(first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, h) %>% 
          rename(
            Jugador = jugador,
            H = h
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table picheo lideres bb ----
      output$p_bb <- renderDataTable({
        
        bb <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(bb)) %>%
          select(first_name, last_name, bb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, bb) %>% 
          rename(
            Jugador = jugador,
            BB = bb
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      # Table picheo lideres era ----
      output$p_era <- renderDataTable({
        
        era <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            era = round((er * 9) / ip, 2),
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
          ) %>% 
          filter(ip > 400) %>% 
          arrange(era) %>%
          select(first_name, last_name, era) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_frac(5, era) %>% 
          rename(
            Jugador = jugador,
            ERA = era
          ) %>% 
          slice(1: n()-1)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          era,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres whip ----
      output$p_whip <- renderDataTable({
        
        whip <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(sum(h, bb) / ip, 2),
            `h/9` = round(mean(`h/9`, na.rm = T), 2),
            `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
            `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
            `so/9` = round(mean(`so/9`, na.rm = T), 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T)
          ) %>% 
          filter(ip > 400) %>% 
          arrange(whip) %>%
          select(first_name, last_name, whip) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          # top_n(5, whip) %>% 
          rename(
            Jugador = jugador,
            WHIP = whip
          ) %>% 
          slice(1:n()-1)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          whip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres sv ----
      output$p_sv <- renderDataTable({
        
        sv <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(sv)) %>%
          select(first_name, last_name, sv) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, sv) %>% 
          rename(
            Jugador = jugador,
            SV = sv
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sv,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres ir ----
      output$p_ir <- renderDataTable({
        
        ir <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
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
          ) %>% 
          arrange(desc(ir)) %>%
          select(first_name, last_name, ir) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(5, ir) %>% 
          rename(
            Jugador = jugador,
            IR = ir
          ) 
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ir,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 5 historico')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres h/9 ----
      output$p_h9 <- renderDataTable({
        
        h_9 <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T)
          ) %>% 
          filter(ip > 400) %>% 
          arrange(`h/9`) %>%
          select(first_name, last_name, `h/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          # top_n(5, `h/9`) %>% 
          rename(
            Jugador = jugador,
            `H/9` = `h/9`
          ) %>% 
          slice(1:n()-1)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres so/9 ----
      output$p_so9 <- renderDataTable({
        
        so_9 <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T)
          ) %>% 
          filter(ip > 400) %>% 
          arrange(desc(`so/9`)) %>%
          select(first_name, last_name, `so/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          # top_n(5, `h/9`) %>% 
          rename(
            Jugador = jugador,
            `SO/9` = `so/9`
          ) %>% 
          slice(1:5)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres bb/9 ----
      output$p_bb9 <- renderDataTable({
        
        bb_9 <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            bk = sum(bk, na.rm = T)
          ) %>% 
          filter(ip > 400) %>% 
          arrange(`bb/9`) %>%
          select(first_name, last_name, `bb/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          # top_n(5, `h/9`) %>% 
          rename(
            Jugador = jugador,
            `BB/9` = `bb/9`
          ) %>% 
          slice(1:5)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
        ) 
      })
      
      # Table picheo lideres so/bb ----
      output$p_sobb <- renderDataTable({
        
        so_bb <- prs() %>% 
          mutate(key = paste(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, ID, first_name, last_name), by = 'key') %>%
          select(ID, first_name,last_name, jugador, 2:29, -key) %>%
          group_by(ID) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            era = round(mean(era, na.rm = T), 2),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            er = sum(er, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = round(mean(whip, na.rm = T), 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(`so/9` / `bb/9`, 2),
            bk = sum(bk, na.rm = T)
          ) %>% 
          filter(ip > 400) %>% 
          arrange(desc(`so/bb`)) %>%
          select(first_name, last_name, `so/bb`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          # top_n(5, `h/9`) %>% 
          rename(
            Jugador = jugador,
            `SO/BB` = `so/bb`
          ) %>% 
          slice(1:5)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con mas de 400 ip')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
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
      # image Batting player ----
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
      
      
      # image Pitching Player ----
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
      
      
      
      # image Record AVG ----
      output$record_avg <- renderImage({
        req(input$select_jugador)
        
        player <- 'www/batting/C. Suarez.jpg'
        
        if (1 > 0) {
          return(list(
            src = 'http://www.milb.com/images/500235/t442/180x270/500235.jpg',
            contentType = "image/jpg",
            width = 150,
            height = 150
            # alt = 'Selecciona un jugador'
          ))
        }
      }, deleteFile = FALSE)
      
      # ----TEXT OUTPUT -----
      # Text output season ----
      output$year <- renderText({
        req(input$select_jugador)
        
        df <- Rosters %>% 
          filter(years == input$select_temporada) %>% 
          select(years) %>% 
          unique() %>% 
          pull()
      })
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
      
      
      
      
    }
  )




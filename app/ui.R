# libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(lubridate)
library(plotly)
library(dplyr)
library(DT)
library(stringr)
# library(bs4Dash)

# Data select ----
.st <- as.Date("2020-01-22")
.en <- as.Date(today())
.dates <- seq(.en, .st, by = "-1 day")
year(today()) -1

# List seasons ---- 
from <- 1962
to <- lubridate::year(Sys.Date()) 
range_ <- c(from:to)
pages <- c(1:(to - (from)))

season <-  function(x){
  df <- paste(range_[x], "-", substring(range_[x + 1], 3), sep="")
  data.frame(df)
}

temporadas <- data.table::rbindlist(
  lapply(pages, season), fill = TRUE
) %>% 
  arrange(desc(df)) %>% 
  rename(temporadas = df) %>% 
  pull()

# APP ----
ui = dashboardPagePlus(
  # Tittle ----
  title = 'Tiburones de la Guaira B.B.C',
  collapse_sidebar = TRUE,
  
  # Header  ----
  header = dashboardHeaderPlus(
    
    # titlePanel(
    #   rel = "icon",
    #   type = "image/gif",
    #   href = "https://guidetoiceland.is/image/389003/x/0/the-beautiful-waterfalls-of-south-iceland-seljalandsfoss-skogafoss-amp-gljufrabui-1.jpg"
    # ),
    title = tagList(
      span(class = "logo-lg", "Tiburones de la Guaira")
      ),
    # (title = span(tagList(icon("calendar"), "Example"))
    # fixed = TRUE,
    titleWidth = 233,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
    
    # User Ruben Lopez
    userOutput("user")
  ),
  
  # Sidebar ----  
  sidebar = dashboardSidebar(
    sidebarMenuOutput('collapsible_sidebar'),
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
  
  
  # Footer ----
  footer = bs4Dash::bs4DashFooter(
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
      target = "_blank"),
    )
  ),
  
  # Body ----
  body = dashboardBody(
    tabItems(
      
      # tabitem Inicio ----
      tabItem(
        tabName = 'inicio',
        h2('Registro Estadístico historico de Tiburones de la Guaira', align = 'center'),
        fluidRow(
          column(12
                 # bs4Dash::bs4Carousel(
                 #   id = "mycarousel",
                 #   width = 6,
                 #   bs4CarouselItem(
                 #     active = TRUE,
                 #     src = "http://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap"
                 #   ),
                 #   bs4CarouselItem(
                 #     active = FALSE,
                 #     src = "http://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap"
                 #   ),
                 #   bs4CarouselItem(
                 #     active = FALSE,
                 #     src = "http://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap"
                 #   )
                 # )
          )
        )
      ),

      # tabItem for Picheo, Bateo and Fildeo of Regular season ----
      tabItem(
        h2('Datos por temporada', align = 'center'),
        tabName = 'temporada',
        tabsetPanel(
          # tabPanel Picheo ----
          tabPanel('Picheo', 
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
          # tabPanel Bateo
          tabPanel('Bateo',
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
                            bs4Dash::bs4Box(
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
                            bs4Dash::bs4Box(
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
                            bs4Dash::bs4Box(
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
          # # tabPanel Fildeo ----
          # tabPanel('Fildeo', tableOutput('fildeo_rs'))
        )
      ),
      # tabItem for Jugador ----
      tabItem(
        tabName = 'jugador',
        tabsetPanel(
          # Picheo ----
          tabPanel(
            title = 'Picheo',
            # Input ----
              fluidRow(
                br(),
                column(3,
                       selectInput(
                         inputId = 'select_jugador_pit',
                         label = 'Seleccione un jugador',
                         choices = distinct_lan
                       )
                )
              ),
            # Image ----
              fluidRow(
                column(12,
                       widgetUserBox(
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
                
                # column(7,
                #        boxPlus(
                #          width = NULL,
                #          title = 'Lanzador',
                #          collapsible = FALSE,
                #          status = 'warning',
                #          closable = FALSE
                #        )
                # ),
                # column(5,
                #        boxPlus(
                #          width = NULL,
                #          collapsible = TRUE,
                #          status = 'warning',
                #          title = 'Radarchart'
                #        )
                # )
              ),
            # Table  ----
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
                     DT::dataTableOutput('picheo_jugador')
                     ),
                   br(),
                   bs4Dash::bs4Box(
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
                   bs4Dash::bs4Box(
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
            title = 'Bateo',
            # Input ----
            fluidRow(
              br(),
              column(3,
                     selectInput(
                       inputId = 'select_jugador',
                       label = 'Seleccione un jugador',
                       choices = distinct_bats
                     )
              )
            ),
            # Image ----
            fluidRow(
              column(7,
                     bs4Dash::bs4Box(
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
                     bs4Dash::bs4Box(
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
                     bs4Dash::bs4Box(
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
                     bs4Dash::bs4Box(
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
                     bs4Dash::bs4Box(
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
          ),
        # -----
      )
    )
  )
)

# End Body ----

# Run app ----
shinyApp(ui, server)



#libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(lubridate)
library(plotly)
library(dplyr)
library(DT)

# Ui main

ui = dashboardPagePlus(
  
  # Tittle ----
  title = 'Tiburones de la Guaira',
  # collapse_sidebar = TRUE, 
  
  # Header  ----
  header = dashboardHeaderPlus(
    # titlePanel(
    #   rel = "icon",
    #   type = "image/gif",
    #   href = "https://guidetoiceland.is/image/389003/x/0/the-beautiful-waterfalls-of-south-iceland-seljalandsfoss-skogafoss-amp-gljufrabui-1.jpg"
    # ),
    title = tagList(
      span(class = "logo-lg", "Tiburones de la Guaira"), 
      img(src = "logo_tiburones.svg")),
    # (title = span(tagList(icon("calendar"), "Example"))
    # fixed = TRUE,
    titleWidth = 250,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
      userOutput("user")
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenuOutput('collapsible_sidebar')),
  # Body ----
  body = dashboardBody(
    includeCSS("www/style.css"),
    tabItems(
      tabItem(
        tabName = 'inicio',
        h1('Registro Estadístico historico de Tiburones de la Guaira', align = 'center')
      ),
      
      # tabItem for Picheo, Bateo and Fildeo of Regular season ----
      tabItem(
        h1('Temporada Regular', align = 'center'),
        tabName = 'tem_reg',
        tabsetPanel(
          # tabPanel Picheo
          tabPanel('Picheo', 
                   # fluidRow(
                   #   column(9),
                   #   column(3,
                   #          box(
                   #            tittle = 'Información',
                   #            solidHeader = TRUE,
                   #            status = 'info',
                   #            collapsible = TRUE,
                   #            width = 12,
                   #            p('Filtre la tabla por cualquier variable', align = 'center')
                   #            )
                   #          ),
                   #   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_rs')
                            )
                     )
                  ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('bateo_rs')
                            )
                     )
                   ),
          # tabPanel Fildeo
          tabPanel('Fildeo', tableOutput('fildeo_rs'))
        )
      ),
      
      # tabItem for Picheo, Bateo in Round robin / semi finals ----
      tabItem(
        h1('Round Robin - Semi Final', align = 'center'),
        tabName = 'rr_sm',
        tabsetPanel(
          # tabPanel Picheo
          tabPanel('Picheo', 
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_rr_sm')
                     )
                   )
          ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('bateo_rr_sm')
                     )
                   )
          )
        )
      ),
      # tabItem for Picheo, Bateo in  finals ---- 
      tabItem(
        h1('Finales', align = 'center'),
        tabName = 'finals',
        tabsetPanel(
          # tabPanel Picheo
          tabPanel('Picheo', 
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_finals')
                     )
                   )
          ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('bateo_finals')
                     )
                   )
          )
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
                column(7,
                       boxPlus(
                         width = NULL,
                         title = 'Lanzador',
                         collapsible = FALSE,
                         status = 'warning',
                         closable = FALSE
                       )
                ),
                column(5,
                       boxPlus(
                         width = NULL,
                         collapsible = TRUE,
                         status = 'warning',
                         title = 'Radarchart'
                       )
                )
              ),
            # Table  ----
              fluidRow(
                column(12,
                       DT::dataTableOutput('picheo_jugador')
                )
              )
            ),
          # Bateo ----
          tabPanel(
            title = 'Bateo',
            # Input ----
            fluidRow(
              column(3,
                     selectInput(
                       inputId = 'select_jugador',
                       label = 'Selecciona un jugador',
                       choices = distinct_bats
                     )
              )
            ),
            # Image ----
            fluidRow(
              column(7,
                     boxPlus(
                       width = NULL,
                       collapsible = TRUE,
                       status = 'warning',
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
                     boxPlus(
                       width = NULL,
                       collapsible = TRUE,
                       status = 'warning',
                       title = 'Radarchart'
                     )
              )
            ),
            # Table  ----
            fluidRow(
              column(12,
                     boxPlus(
                       width = NULL,
                       title = 'Temporada Regular',
                       collapsible = TRUE,
                       closable = FALSE,
                       status = 'warning',
                       DT::dataTableOutput('bat_rs')
                       ),
                     boxPlus(
                       width = NULL,
                       title = 'Round Robin',
                       collapsible = TRUE,
                       closable = FALSE,
                       status = 'warning',
                       DT::dataTableOutput('bat_rr')
                       ),
                     boxPlus(
                       width = NULL,
                       title = 'Finales',
                       align = 'center',
                       collapsible = TRUE,
                       closable = FALSE,
                       status = 'warning',
                       DT::dataTableOutput('bat_final')
                       ) 
                )
              )
            )
          ),
        # ----
      )
    )
  )
)

# End Body ----





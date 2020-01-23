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
  
  # Herader ----
  header = dashboardHeaderPlus(
    # titlePanel(
    #   rel = "icon",
    #   type = "image/gif",
    #   href = "https://guidetoiceland.is/image/389003/x/0/the-beautiful-waterfalls-of-south-iceland-seljalandsfoss-skogafoss-amp-gljufrabui-1.jpg"
    # ),
    title = tagList(
      span(class = "logo-lg", "Tiburones de la Guaira"), 
      img(src = "andreas_Bureau_de_change.svg")),
    # (title = span(tagList(icon("calendar"), "Example"))
    # fixed = TRUE,
    titleWidth = 250,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
    userOutput("user")
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    br(),
      # meniItem Tiburones de la Guaira ----
       menuItem(
         text = 'Tiburones de la Guaira',
         tabName = 'inicio',
         badgeLabel = "new", 
         badgeColor = "green",
         icon = icon('text-height', lib = 'font-awesome')
         ),
      br(),
      # menuItem  Estadisticas ----
        menuItem(
          'Estadísticas',
          tabName = 'estadisticas',
          icon = icon('chart-line', lib = 'font-awesome'),
          menuSubItem('Temporada Regular', tabName = 'tem_reg'),
          menuSubItem('Round Robin / Semi final', tabName = 'rr_sm'),
          menuSubItem('Finales', tabName = 'finals'),
          menuSubItem('Por Jugador', tabName = 'jugador')
        ),
      br(),
      # menuItem Geo Estadisticas ----
        menuItem(
          'Geo Estadísticas',
          startExpanded = FALSE,
          tabName = 'geo_estadisticas',
          icon =  icon('globe-americas', lib = 'font-awesome'),
          menuSubItem('Geograficas', tabName = 'geo'),
          menuSubItem('Caracateristicas', tabName = 'hab')
        ),
      br(),
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
      br(),
      # menuItem Historia ----
        menuItem(
          'Historia',
          tabName = 'historia',
          icon = icon('search-location', lib = 'font-awesome'),
          menuSubItem('En números', tabName = 'en_num'),
          menuSubItem('Estadio', tabName = 'rr_sm')
        ),
      br(),
      # menuItem Glosario ----
        menuItem(
          'Glosario',
          tabName = 'glosario',
          icon = icon('google', lib = 'font-awesome'),
          menuSubItem('Glosario Sabermetrico', tabName = 'g_saberm'),
          menuSubItem('Cálculos', tabName = 'calc')
        )
  ),
  
  # Body ----
  body = dashboardBody(
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
                   fluidRow(
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                              )
                            ),
                     ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_rs')
                            )
                     )
                  ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                            )
                     ),
                   ),
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
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                            )
                     ),
                   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_rr_sm')
                     )
                   )
          ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                            )
                     ),
                   ),
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
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                            )
                     ),
                   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput('picheo_finals')
                     )
                   )
          ),
          # tabPanel Bateo
          tabPanel('Bateo',
                   fluidRow(
                     column(9),
                     column(3,
                            box(
                              tittle = 'Información',
                              solidHeader = TRUE,
                              status = 'info',
                              collapsible = TRUE,
                              width = 12,
                              p('Filtre la tabla por cualquier variable', align = 'center')
                            )
                     ),
                   ),
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
        h1('Ruben López', align = 'center'),
        tabName = 'jugador',
        # fluidRow(
        #   column(8,
        #          box(
        #            width = 12
        #          )
        #          ),
        #   column(4,
        #          box(
        #            width = 12
        #          )
        #          )
        # ),
        # Table test align fluirow ----
        fluidRow(
          column(10),
          column(2,
                 selectInput(
                   inputId = 'select_jugador',
                   label = 'Elige un jugador',
                   choices = distinct_players
                 )
          )
        ),
        # Payer image ----
        fluidRow(
          column(6,
                 boxPlus(
                   width = NULL,
                   # height = 250,
                   title = 'Nombre completo del jugador',
                   collapsible = TRUE,
                   status = 'warning',
                   imageOutput('jugador_',
                               click = 'image_click'
                               # height  = 
                               ) 
                 )
          ),
          # Individual stats ----
          column(6,
                 boxPlus(
                   title = "Exp: 5 Temporadas",
                   align = 'center',
                   status = "warning",
                   width = NULL,
                   collapsible = TRUE,
                   fluidRow(
                     # Stats primary ----
                     column(6,
                            valueBoxOutput(outputId = 'pais', width = NULL),
                            valueBoxOutput(outputId = 'estado', width = NULL),
                            valueBoxOutput(outputId = 'ciudad', width = NULL)
                     ),
                     # Stats secundary ----
                     column(6,
                              valueBoxOutput(outputId = 'lan', width = NULL),
                              valueBoxOutput(outputId = 'pos', width = NULL),
                              valueBoxOutput(outputId = 'bat', width = NULL)
                          )
                       )
                    )
                 )
        ),
        # Table by player ----
        fluidRow(
          column(12,
                 DT::dataTableOutput('info_jugador')
                 )
        )
      )
    )
  )
)

# End Body ----



# tabItem(
#   h1('Jugador', align = 'center'),
#   tabName = 'por_jugador',
#   tabsetPanel(



#     # tabPanel Data 
#     tabPanel('info_pj',
#              fluidRow(
#                column(9),
#                column(3,
#                       box(
#                         tittle = 'Información',
#                         solidHeader = TRUE,
#                         status = 'info',
#                         collapsible = TRUE,
#                         width = 12,
#                         p('Filtre la tabla por cualquier variable', align = 'center')
#                       )
#                ),
#              ),
#              fluidRow(
#                column(4,
#                       imageOutput('jugador',
#                                   height = 300,
#                                   click = 'image_click'
#                       )
#                ),
#                column(8,
#                       DT::dataTableOutput('info_jugador'
#                       )
#                )
#              )
#     )
#   )
# )
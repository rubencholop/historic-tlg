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
    sidebarMenuOutput('collapsible_sidebar')),
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
            h1(textOutput('jugador_lan')
               , align = 'center'),
            # Table test align fluirow ----
              fluidRow(
                column(10),
                column(2,
                       selectInput(
                         inputId = 'select_jugador_pit',
                         label = 'Elige un jugador',
                         choices = distinct_lan
                       )
                )
              ),
            # Payer image ----
              fluidRow(
                column(6,
                       boxPlus(
                         width = NULL,
                         # height = 250,
                         title = '',
                         collapsible = TRUE,
                         status = 'warning',
                         imageOutput('jugador_pit',
                                     click = 'image_click'
                                     # height  = 
                         ) 
                       )
                ),
                # Individual stats ----
                column(6,
                       boxPlus(
                         title = "",
                         align = 'center',
                         status = "warning",
                         width = NULL,
                         collapsible = TRUE,
                         fluidRow(
                           # Stats primary ----
                           column(6,
                                  valueBoxOutput(outputId = 'pais_pit', width = 12),
                                  valueBoxOutput(outputId = 'estado_pit', width = 12),
                                  valueBoxOutput(outputId = 'ciudad_pit', width = 12)
                           ),
                           # Stats secundary ----
                           column(6,
                                  valueBoxOutput(outputId = 'lan_pit', width = 12),
                                  valueBoxOutput(outputId = 'pos_pit', width = 12),
                                  valueBoxOutput(outputId = 'bat_pit', width = 12)
                           )
                         )
                       )
                )
              ),
            # Table by player ----
              fluidRow(
                column(12,
                       DT::dataTableOutput('picheo_jugador')
                )
              )
            ),
          tabPanel(
            # Bateo ----
            title = 'Bateo',
            h1(textOutput('jugador_bat')
               , align = 'center'),
            # Table test align fluirow ----
            fluidRow(
              column(10),
              column(2,
                     selectInput(
                       inputId = 'select_jugador',
                       label = 'Elige un jugador',
                       choices = distinct_bats
                     )
              )
            ),
            # Payer image ----
            fluidRow(
              column(6,
                     boxPlus(
                       width = 12,
                       # height = 250,
                       title = '',
                       collapsible = TRUE,
                       status = 'warning',
                       fluidRow(
                         column(12,
                                imageOutput('jugador_')
                         )
                       )
                     )
              ),
              # Individual stats ----
              column(6,
                     boxPlus(
                       title = "",
                       align = 'center',
                       status = "warning",
                       width = NULL,
                       collapsible = TRUE,
                       fluidRow(
                         # Stats primary ----
                         column(6,
                                valueBoxOutput(outputId = 'pais', width = 12),
                                valueBoxOutput(outputId = 'estado', width = 12),
                                valueBoxOutput(outputId = 'ciudad', width = 12)
                         ),
                         # Stats secundary ----
                         column(6,
                                valueBoxOutput(outputId = 'lan', width = 12),
                                valueBoxOutput(outputId = 'pos', width = 12),
                                valueBoxOutput(outputId = 'bat', width = 12)
                         )
                       )
                     )
              )
            ),
            # Table by player ----
            fluidRow(
              column(12,
                     # br(),
                     h1('Bateo en temporada regular', align = 'center'),
                     DT::dataTableOutput('bat_rs'),
                     # br(),
                     h1('Bateo en round robin - postemporada', align = 'center'),
                     DT::dataTableOutput('bat_rr'),
                     # br(),
                     h1('Bateo en finales', align = 'center'),
                     DT::dataTableOutput('bat_final')
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


# 


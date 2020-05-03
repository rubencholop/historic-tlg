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
      span(class = "logo-lg", "Tiburones de la Guaira"), 
      img(src = "tiburones-de-la-guaira-bbc.eps")),
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
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
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
                }'
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
          column(12,
                 bs4Dash::bs4Carousel(
                   id = "mycarousel",
                   width = 12,
                   bs4Dash::bs4CarouselItem(
                     active = TRUE,
                     src = "wash.png"
                   ),
                   bs4Dash::bs4CarouselItem(
                     active = FALSE,
                     src = "market.png"
                   ),
                   bs4Dash::bs4CarouselItem(
                     active = FALSE,
                     src = "steps.png"
                     # http://placehold.it/900x500/f39c12/ffffff&text=I+Love+Bootstrap
                   )
                 )
          )
        )
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





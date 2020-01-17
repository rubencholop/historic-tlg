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
  header = dashboardHeaderPlus(),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem('Tiburones de la Guaira',
               tabName = 'inicio',
               icon = icon('shark')
      ),
      sidebarMenu(
        menuItem(
          'Estadísticas',
          startExpanded = TRUE,
          tabName = 'estadisticas',
          icon = icon('dashboard'),
          menuSubItem('Temporada Regular', tabName = 'tem_reg'),
          menuSubItem('Round Robin / Semi final', tabName = 'rr_sm'),
          menuSubItem('Finales', tabName = 'finales'),
          menuSubItem('Por Jugador', tabName = 'jugador')
        )
      )
    )
  ),
  
  # Body ---
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
          # id = 'picheo',
          tabPanel('Picheo', 
                   fluidRow(
                     column(12,
                            box(
                              selectInput(
                                inputId =  'season',
                                label = 'Elige una temporada',
                                choices = distinct_years
                              )
                            )
                     )
                   ),
                   
                  fluidRow(
                    column(12,
                      box(
                          DT::dataTableOutput('picheo_rs')
                          )
                      )
                    )
                  ),
          tabPanel('Bateo', tableOutput('bateo_rs')),
          tabPanel('Fildeo', tableOutput('fildeo_rs'))
        )
      )
    )
  )
  
)
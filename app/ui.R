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
      )
    )
  )
  
)
#libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(lubridate)
library(plotly)
library(dplyr)
library(DT)


# Ui ----
# shinyApp(
ui = dashboardPagePlus(
  
  # Tittle ----
  title = "Tiburones de la Guaira",
  # skin = "black",
  
  # Header ----
  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    left_menu = tagList(
      dropdownBlock(
        id = "mydropdown",
        title = "Dropdown 1",
        icon = "sliders",
        sliderInput(
          inputId = "n",
          label = "Number of observations",
          min = 10, max = 100, value = 30
        ),
        prettyToggle(
          inputId = "na",
          label_on = "NAs keeped",
          label_off = "NAs removed",
          icon_on = icon("check"),
          icon_off = icon("remove")
        )
      ),
      dropdownBlock(
        id = "mydropdown2",
        title = "Dropdown 2",
        icon = "sliders",
        prettySwitch(
          inputId = "switch4",
          label = "Fill switch with status:",
          fill = TRUE,
          status = "primary"
        ),
        prettyCheckboxGroup(
          inputId = "checkgroup2",
          label = "Click me!",
          thick = TRUE,
          choices = c("Click me !", "Me !", "Or me !"),
          animation = "pulse",
          status = "info"
        )
      )
    ),
    userOutput("user")
  ),
  
  # Side bar ----
  sidebar = dashboardSidebar(
    # menuItem Inicio ----
    sidebarMenu(
      menuItem('Tiburones de la Guaira',
               tabName = 'inicio',
               icon = icon('shark')
      ),
      
    # menuItem Estadisticas ----
      menuItem('Estadísticas',
               tabName = 'estadisticas',
               icon = icon('dashboard'),
               startExpanded = TRUE,
               menuSubItem('Temporada Regular', tabName = 'tem_reg'),
               menuSubItem('Round Robin / Semi final', tabName = 'rr_sm'),
               menuSubItem('Finales', tabName = 'finales'),
               menuSubItem('Por Jugador', tabName = 'jugador')
               
      )
    ),
    # menuItem Estadisticas geograficas ----
    sidebarMenu(
      menuItem(
        'Geo Estadísticas',
        tabName = 'g_estadisticas',
        icon = icon('stats'),
        menuSubItem('Geograficas', tabName = 'geo'),
        menuSubItem('Caracateristicas', tabName = 'hab')
      )
    ),
    # menuItem Records ----
    sidebarMenu(
      menuItem(
        'Records',
        tabName = 'records',
        icon = icon('up'),
        menuSubItem('Historicos', tabName = 'historicos'),
        menuSubItem('Por temporadas', tabName = 'p_tem'),
        menuSubItem('Records en LVBP', tabName = 'lvbp'),
        menuSubItem('Sabermetria', tabName = 'saberm')
      )
    ),
    # menuItem Historia ----
    sidebarMenu(
      menuItem(
        'Historia',
        tabName = 'historia',
        icon = icon('dashboard'),
        menuSubItem('En números', tabName = 'en_num'),
        menuSubItem('Estadio', tabName = 'rr_sm')
      )
    ),
    # menuItem Glosario ----
    sidebarMenu(
      menuItem(
        'Glosario',
        tabName = 'glosario',
        icon = icon('dashboard'),
        menuSubItem('Glosario Sabermetrico', tabName = 'g_saberm'),
        menuSubItem('Cálculos', tabName = 'calc')
      )
    )
  ),
  
  
  # Body ----
  body = dashboardBody(
    # TabItem Estadisticas ----
    tabItems(
      # tabItem Inicio ----
      tabItem(
        tabName = 'inicio',
        h1('Registro Estadístico historico de Tiburones de la Guaira', align = 'center')
      ),
      
      # tabItem Temporada regular ----
      tabItem(
        h1('Temporada Regular', align = 'center'),
        tabName = 'tem_reg',
        tabsetPanel(
          # id = 'picheo',
          tabPanel('Picheo', DT::dataTableOutput('mytable1')),
          tabPanel('Bateo', tableOutput('1321321')),
          tabPanel('Fildeo', tableOutput('132asdsadasd'))
        )
      ),
      # tabItem Roun robin ----
      tabItem(
        h1('Round Robin / Semi Finales', align = 'center'),
        tabName = 'rr_sm',
        tabsetPanel(
          id = 'picheo',
          tabPanel(
            'Picheo',
            # valeu = 'page1',
            fluidRow(
              column(2,
                     tableOutput('Table')
              )
            )
          ),
          tabPanel(
            'Bateo',
            tableOutput('1321321')
          )
        )
      ),
      
      # tabItem Finales ----
      tabItem(
        h1('Finales', align = 'center'),
        tabName = 'finales',
        tabsetPanel(
          id = 'picheo',
          tabPanel(
            'Picheo',
            # valeu = 'page1',
            fluidRow(
              column(2,
                     tableOutput('Table')
              )
            )
          ),
          tabPanel(
            'Bateo',
            tableOutput('1321321')
          )
        )
      ),
      
      # tabItem Por jugador ----
      tabItem(
        h1('Estadísticas Individuales', align = 'center'),
        tabName = 'jugador',
        tabsetPanel(
          id = 'picheo',
          tabPanel(
            'Picheo',
            # valeu = 'page1',
            fluidRow(
              column(2,
                     tableOutput('Table')
              )
            )
          ),
          tabPanel(
            'Bateo',
            tableOutput('1321321')
          )
        )
      )
    ),
    
    # TabItem Geo Estadisticas ----
    tabItems(
      tabItem(tabName = 'g_estadisticas')
    ),
    
    # TabItem Records ----
    tabItems(
      tabItem(tabName = 'records')
    ),
    
    # TabItem Historia ----
    tabItems(
      tabItem(tabName = 'hisroria')
    ),
    
    # TamItem Glosario ----
    tabItems(
      tabItem(tabName = 'glosario')
    )
  ),
  
  # Right bar ----
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      icon = "desktop",
      active = TRUE,
      title = "Tab 1"
    ),
    rightSidebarTabContent(
      id = 2,
      title = "Tab 2",
      textInput("caption", "Caption", "Data Summary")
    ),
    rightSidebarTabContent(
      id = 3,
      icon = "paint-brush",
      title = "Tab 3",
      numericInput("obs", "Observations:", 10, min = 1, max = 100)
    )
  ),
  
  # Footer ----
  footer = dashboardFooter(
    left_text = "Fuente: www.pelotabinaria.com",
    right_text = 'Caracas - 2020'
  )
)


# Server ----
server = function(input, output) { 
  
  # Example User ----
  output$user <- renderUser({
    dashboardUser(
      name = "Rubén López",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      title = "Sports Data Analytics",
      subtitle = "",
      footer = p("DASHBOARD HITORICO DE TIBURONES DE LA GUAIRA", class = "text-center"),
      fluidRow(
        dashboardUserItem(
          width = 4,
          socialButton(
            url = "https://www.linkedin.com/in/ruben-lopez-28002bb4/",
            type = "linkedin"
          )
        ),
        dashboardUserItem(
          width = 4,
          socialButton(
            url = "https://github.com/ruhanslop",
            type = "github"
          )
        ),
        dashboardUserItem(
          width = 4,
          socialButton(
            url = "https://github.com/ruhanslop",
            type = "instagram"
          )
        )
      )
    )
  })
  
  # Data raw reactive -----
  pitching_full_tem = reactive({
    # req(inpunt$season)
    
    df <- Hprs %>% 
      # filter(
      #   # years == input$season
      # ) %>% 
      select(years, 3:28)
  })
  
  # Data tables Picheo tem
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(mtcars)
  })
  
  # df %>% 
  #   select(years, 3:10)
  # pitching_full_tem() %>% 
  #   select(years, 3:8)
  # options = list(
  #   scrollX = TRUE,
  #   columnDefs = list(
  #     list(
  #       className = 'dt-center',
  #       targets = 0:4)
  #     )
  #   )
  
  
  # column(3,
  #        selectInput(
  #          'season',
  #          label = 'Elige una temporada',
  #          choices = distinct_years,
  #          selected = distinct_years 
  #          )
  #        )
  
  # Ends of Server ----
}
# )


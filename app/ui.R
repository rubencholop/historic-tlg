#libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

# Ui
shinyApp(
  ui = dashboardPagePlus(
    
    # Header ----
    header = dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"
    ),
    
    # Side bar ----
    sidebar = dashboardSidebar(),
    
    # Body ----
    body = dashboardBody(),
    
    # Right bar ----
    rightsidebar = rightSidebar(
      background = "dark"
    ),
    
    # Footer ----
    footer = dashboardFooter(
      left_text = "By Rubén López \n
                  (Sports Data Analytics)",
      right_text = "Caracas, 2020"
    ),
    
    # Tittle ----
    title = "Tiburones de la Guaira"
  ),
  
  # Server ----
  server = function(input, output) { }
)


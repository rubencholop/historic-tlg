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


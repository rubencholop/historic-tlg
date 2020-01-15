#libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

# Ui
shinyApp(
  ui = dashboardPagePlus(
    
    # Tittle ----
    title = "Tiburones de la Guaira",
    
    # Header ----
    header = dashboardHeaderPlus(
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears",
      left_menu = NULL,
      userOutput("user")
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
    )
  ),
  
  # Server ----
  server = function(input, output) { 
  
# Example  User ----
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
}
)


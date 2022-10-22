# Libraries ----
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(dplyr)
library(DT)
# library(stringr)
library(readr)
library(highcharter)
library(shinyalert)
library(renv)
library(tweetrmd)
library(rtweet)
# library(stringi) 
# library(shinyflags)

# Functions shiny ----

userList <- function(...) {
  shiny::tags$ul(
    class = "users-list clearfix",
    ...
  )
}


userListItem <- function(image, title, title2, subtitle = NULL) {
  shiny::tags$li(
    shiny::tags$img(
      src = image, 
      alt = "User Image",
      shiny::tags$a(class = "users-list-name", title),
      if (!is.null(subtitle)) {
        shiny::tags$span(class = "users-list-date", subtitle)
      }
    )
  )
}

# Choices ----

Rosters <- read_csv('data/rosters_clean.csv')
PitchingLog <- read_csv('data/pitching_log.csv')
BattingLog <- read_csv('data/batting_log.csv')

.pitching_log <- PitchingLog %>% 
  janitor::clean_names() %>% 
  select(temporada) %>% 
  unique() %>% 
  pull()


.batting_log <- BattingLog %>% 
  janitor::clean_names() %>% 
  select(temporada) %>% 
  unique() %>% 
  pull()

.rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  distinct(name) %>% 
  mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
  )

Rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  left_join(.rosters, by = 'name')

# unique roster
Unique_Rosters <- Rosters %>% 
  group_by(ID) %>% 
  summarize(
    jugador = last(jugador),
    name = last(name),
    pos = last(pos),
    bat = last(bat),
    lan = last(lan),
    exp = last(exp),
    pais = last(pais),
    estado = last(estado),
    ciudad = last(ciudad),
    .groups = 'drop'
  ) %>% 
  ungroup() %>% 
  arrange(jugador) 

.paises <- Rosters %>% 
  group_by(pais) %>% 
  summarize(pais = last(pais),
            .groups = 'drop') %>% 
  arrange(pais) %>% 
  pull()

.paises_batting <- Rosters %>% 
  filter(pos != "P") %>% 
  group_by(pais) %>% 
  summarize(pais = last(pais),
            .groups = 'drop') %>% 
  arrange(pais) %>% 
  pull()

.paises_pitching <- Rosters %>% 
  filter(pos == "P") %>% 
  group_by(pais) %>% 
  summarize(pais = last(pais),
            .groups = 'drop') %>% 
  arrange(pais) %>% 
  pull()


.ciudad <- Rosters %>% 
  group_by(ciudad) %>% 
  summarize(ciudad = last(ciudad),
            .groups = 'drop') %>% 
  arrange(ciudad) %>% 
  pull()

.pitchers <- Rosters %>% 
  filter(pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = paste0(last(first_name), " ", last(last_name)),
            .groups = 'drop') %>% 
  arrange(jugador) %>% 
  select(jugador) %>% 
  pull()

.bateadores <- Rosters %>% 
  filter(!pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = paste0(last(first_name), " ", last(last_name)),
            .groups = 'drop') %>% 
  arrange(jugador) %>% 
  select(jugador) %>% 
  pull()

posiciones <- Rosters %>% 
  select(pos) %>% 
  mutate(pos = if_else(pos == "p", "P", pos)) %>% 
  unique() %>% 
  pull()




.st <- as.Date("2020-01-22")
.en <- as.Date(today())
.dates <- seq(.en, .st, by = "-1 day")
year(today()) -1

# Dates 
from <- 1962
to <- lubridate::year(Sys.Date())+1 
range_ <- c(from:to)
pages <- c(1:(to - (from)))

season <-  function(x){
  df <- paste(range_[x], "-", substring(range_[x + 1], 3), sep = "")
  data.frame(df)
}

temporadas <- data.table::rbindlist(
  lapply(pages, season), fill = TRUE
) %>% 
  arrange(desc(df)) %>% 
  rename(temporadas = df) %>% 
  pull() %>% as.character()
# %>% 
#   pull()

temporadas_asc <- data.table::rbindlist(
  lapply(pages, season), fill = TRUE
) %>% 
  arrange(df) %>% 
  rename(temporadas = df) %>% 
  pull() %>% as.character()


# Special datos to some metrics with OBP, OPS and SLG
from1 <- 2005
to1 <- lubridate::year(Sys.Date()) 
range_1 <- c(from1:to1)
pages1 <- c(1:(to - (from1)))

season1 <-  function(x){
  df <- paste(range_1[x], "-", substring(range_1[x + 1], 3), sep = "")
  data.frame(df)
}

temporadas_batting <- data.table::rbindlist(
  lapply(pages1, season1), fill = TRUE
) %>% 
  arrange(desc(df)) %>% 
  rename(temporadas = df) %>% 
  pull()

# Functions ----
IP <- function(x){
  x <- x %>%
    as.character()
  
  episodio <-  as.numeric(sub("\\..*", "", x))
  episodio <- sum(episodio)
  
  tercio <- dplyr::case_when(stringr::str_detect(x,"\\.") ~ stringr::str_sub(x, -1, -1),
                      TRUE ~ "0")
  tercio <- as.numeric(tercio)
  tercio <- sum(tercio)
  
  x <-  episodio + trunc(tercio / 3) + ((tercio %% 3) / 10)

  return(x)
}

leaders <- function(stat, .ip = 0){
  .roster <- Rosters
  .roster <- dplyr::select(Rosters, player_id, years, name, ID, first_name, last_name)
  
  
  .prs <- prs
  .prs <- dplyr::filter(.prs, ronda == "regular")
  .prs <- dplyr::mutate(.prs, key = paste0(as.character(years), jugador, sep = ""))
  .prs <- dplyr::select(.prs, player_id, 1:28)
  .prs <- dplyr::left_join(.prs, .roster, by = c("player_id", "years"))
  .prs <- dplyr::select(.prs, player_id, first_name,last_name, jugador, 2:29)
  .prs <- dplyr::group_by(.prs, player_id) 
  .prs <- dplyr::summarise(.prs,
                           first_name = dplyr::last(first_name),
                           last_name = dplyr::last(last_name),
                           jugador= dplyr::last(jugador),
                           w = sum(w, na.rm = T),
                           l = sum(l, na.rm = T),
                           er = sum(er, na.rm = T),
                           ip = IP(ip),
                           era = as.character(round((er * 9) / ip, 2)),
                           g = sum(g, na.rm = T),
                           gs = sum(gs, na.rm = T),
                           cg = sum(cg, na.rm = T),
                           sho = sum(sho, na.rm = T),
                           sv = sum(sv, na.rm = T),
                           h = sum(h, na.rm = T),
                           r = sum(r, na.rm = T),
                           hr = sum(hr, na.rm = T),
                           bb = sum(bb, na.rm = T),
                           so = sum(so, na.rm = T),
                           ir = sum(ir, na.rm = T),
                           whip = as.character(round(mean(whip, na.rm = T), 2)),
                           `h/9` = as.character(round((h/ip)*9, 2)),
                           `hr/9` = as.character(round((hr/ip)*9, 2)),
                           `bb/9` = as.character(round((bb/ip)*9, 2)),
                           `so/9` = as.character(round((so/ip)*9, 2)),
                           `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
                           img = '<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>',
                           .groups = 'drop')        
  .prs <- dplyr::filter(.prs, ip > .ip) 
  .prs <- dplyr::arrange(.prs, desc(stat))
  .prs <- dplyr::select(.prs, img, first_name, last_name, stat)
  .prs <- tidyr::unite(.prs, 'jugador', first_name, last_name, sep = ' ')
  .prs <- dplyr::top_n(.prs, 10, stat) 
  .prs <- dplyr::rename(.prs,
                        " " = img,
                        Jugador = jugador
  )
  
  if (nrow(.prs) >= 11) {
    .prs <- dplyr::slice(.prs, 1:(n()-1))
  } else {
    .prs
  }
  
  .prs
  
}

  shiny::shinyApp(
# Page ----
    ui = bs4DashPage(
      # tags$head(includeHTML(("google-analytics.html"))),
      enable_preloader = TRUE, # Icon before preloader
      controlbar_overlay = TRUE,
      sidebar_collapsed = TRUE,
      # Navbar ----
      navbar = bs4DashNavbar(
        status = "danger",
        border = TRUE,
        controlbarIcon = "th",
        fixed = TRUE,
        tags$head(
          tags$style(
            HTML('
            .main-sidebar{
              background-color: #011C51;
            }

            .brand-link{
            border bottom; none;
            }

            .bg-dark{
            background-color: #0473cb;
            }

            .navbar-light {
            background-color: #f4f6f9;
            }

           ')
          )
        ),
        # column(2),
        # column(8, h2("Registro historico de Tiburones de la Guaira", 
        #               style = "color: #707070; text-align: left; 
        #                       font-size: 1.6rem; font-family: 'Roboto Regular', sans-serif;
        #                       font-weight: 500; text-transform: uppercase;",
        #                           align = 'center'),
        #                     style = "padding-left: 0px;"),
        # column(1),
        # div(id = "tiburones",
        #     h2("Registro historico de Tiburones de la Guaira", 
        #        style = "color:red", align = 'center'),
        #     tags$style(HTML('#tiburones {align-left: auto;}'))
        # ),
        # column(2, tags$img(src = "logo_wahoo2.png",  width = '95px')),
        # column(1),
        # column(1, tags$img(src = "logo_nbs3.png",  width = '75px', align = "right",
        #                    style = "padding-right: 0px;")
        # ),
        # column(5, h2("Showcase November 2nd to 4th", style = "color: #707070; text-align: left; 
        #                                        font-size: 1.3rem; font-family: 'Roboto Regular', sans-serif;
        #                                         font-weight: 500; text-transform: uppercase;",
        #              align = 'Left'),
        #        style = "padding-left: 0px;"),
        # column(3),
        # titleWidth = 1050,
        enable_rightsidebar = TRUE
      ),
      # Sidebar ----
      sidebar = bs4DashSidebar(
        tags$head(tags$style(HTML(
          "[class*='sidebar-dark'] .brand-link {
            border-bottom: none; 
           }
          
          [class*='sidebar-dark-'] .sidebar a {
            color: #FFFF
            }
          "
        ))),
        disable = FALSE,
        title = h4("TIBU STATS",
                   style = "color: #FFFF;
                            # text-transform: uppercase;
                            font-size: 1.2em;
                            text-shadow:1px 1px 2px rgba(150, 150, 150, 1);",
                   align = 'center'),
        skin = "dark",
        status = "primary",
        brandColor = '#0d3583',
        url = NULL,
        src = "ts_isotipo.png",
        elevation = 4,
        opacity = 0.8,
        expand_on_hover = TRUE,
        fixed = TRUE,
        sidebarMenu(
          # meniItem Tiburones de la Guaira ----
          bs4SidebarMenu(
            bs4SidebarMenuItem(
            text = 'Inicio',
            tabName = 'inicio',
            # badgeLabel = "new", 
            # badgeColor = "green",
            icon = "home",
            startExpanded = TRUE
          ),
          # MenuSubItem Datos ----
          bs4SidebarMenuItem(
            text = 'Datos',
            tabName = 'datos',
            icon = "database",
            startExpanded = FALSE,
            bs4SidebarMenuSubItem(text = 'Equipo', tabName = 'equipo', icon = "angle-right"),
            bs4SidebarMenuSubItem(text = 'Temporada', tabName = 'temporada', icon = "angle-right"),
            bs4SidebarMenuSubItem(text = 'Jugador', tabName = 'jugador', icon = "angle-right"),
            bs4SidebarMenuSubItem(text = 'Paises', tabName = 'paises', icon = "angle-right"),
            bs4SidebarMenuSubItem(text = 'Posicion', tabName = 'posicion', icon = "angle-right"),
            bs4SidebarMenuSubItem(text = 'Roster', tabName = 'roster', icon = "angle-right"),
            bs4SidebarMenuSubItem('Geograficas', tabName = 'geograficas', icon = "angle-right"),
            bs4SidebarMenuSubItem('Vzla vs Importados', tabName = 'vzla', icon = "angle-right")
            )
          ),
          # menuItem Records ----
          bs4SidebarMenuItem(
            text = 'Records',
            tabName = 'records',
            icon = "medal",
            bs4SidebarMenuSubItem('De por vida', tabName = 'deporvida', icon = "angle-right"),
            bs4SidebarMenuSubItem('Por temporadas', tabName = 'por_temporadas', icon = "angle-right")
            # bs4SidebarMenuSubItem('Premios en la LVBP', tabName = 'lvbp', icon = "angle-right")
            # bs4SidebarMenuSubItem('Sabermetria', tabName = 'saberm', icon = "angle-right")
          ),
          # menuItem Premios ----
          bs4SidebarMenuItem(
            text = 'Premios',
            tabName = 'premios',
            icon = "trophy",
            bs4SidebarMenuSubItem('Premios en la LVBP', tabName = 'lvbp', icon = "angle-right"),
            bs4SidebarMenuSubItem('Números retirados', tabName = 'retirados', icon = "angle-right"),
            bs4SidebarMenuSubItem('Salón de la Fama', tabName = 'fama', icon = "angle-right")
          ),
          # menuItem History ----
          bs4SidebarMenuItem(
            text ='Historia',
            tabName = 'historia',
            icon = "hourglass",
            bs4SidebarMenuSubItem('Tiburones de la Guaira', tabName = 'en_num', icon = "angle-right"),
            bs4SidebarMenuSubItem('Estadio', tabName = 'rr_sm', icon = "angle-right")
          ),
          # menuItem Graficos ----
          # bs4SidebarMenuItem(
          #   text ='Gráficos',
          #   tabName = 'graficos',
          #   icon = "chart-pie"
          # ),
          # menuItem Glosary ----
          bs4SidebarMenuItem(
            text = 'Glosario',
            tabName = 'glosario',
            icon = "book",
            bs4SidebarMenuSubItem('Estadistícas', tabName = 'g_stat', icon = "angle-right"),
            bs4SidebarMenuSubItem('Sabermetría', tabName = 'sabermetrics', icon = "angle-right")
          ),
          # menuItem Advanced Search ----
          bs4SidebarMenuItem(
            text = 'Busqueda Avanzada',
            tabName = 'busqueda',
            icon = "search-plus"
          ),
          # menuItem About us ----
          bs4SidebarMenuItem(
            text = 'Nosotros',
            tabName = 'nosotros',
            icon = "user-friends"
          )
        ),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
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
      # Control bar ----
      # controlbar = bs4DashControlbar(
      #   disable = TRUE
      # ),
      # Footer ----
      footer = bs4DashFooter(
        tags$head(
          tags$style(
            HTML('
            .main-footer {
              background-color: #f4f6f9;
              border-top: none;
                }
           .img {
              max-width: 15%;
              vertical-align: bottom;
           }
           '
            )
          )
        ),
        # Social networks ----
        fluidRow(
          column(5),
          column(2,
                 div(
                   id = "logo",
                   img(src = 'TS Horizontal Color (2).png', aling = "center")
                   )
                 ),
          column(3),
          column(2,
                 tags$div(
                   class = "portrait-title",
                   tags$ul(
                     class = "network-icon",
                     tags$li(
                       tags$a(href = "https://twitter.com/tibu_stats",
                              tags$i(class = "fab fa-twitter big-icon"),
                              style = "border-left-style: solid;border-left-width: 0px;padding-left: 
                              10px;padding-right: 10px; font-size:1.5rem"
                              )
                       ),
                     tags$li(
                       tags$a(href = "https://www.instagram.com/rubencholop/",
                              tags$i(class = "fab fa-instagram big-icon"),
                              style = "border-left-style: solid;border-left-width: 0px;padding-left: 
                              10px;padding-right: 10px; font-size:1.5rem;"
                              )
                       )
                     )
                   )
                 )
        ),
        # Source Pelotabinaria ----
        fluidRow(
          column(6,
                 span("Fuente de datos:"),
                 a(href = "http://www.pelotabinaria.com.ve/beisbol/", target = "_blank", "http://www.pelotabinaria.com.ve/")
          ),
          column(6)
          ),
        br(),
        # Derechos reservados ----
        fluidRow(
          column(2),
          column(8,
                 h6("© 2022 Tibu Stats. Todos los derechos reservados.", align = "center")
                 ),
          column(2)
          )
        ),
      # Title ----
      title = 'Tiburones de la Guaira B.B.C',
      # Body ----
      body = bs4DashBody(
        tags$head(shiny::includeHTML(("google-analytics.html"))),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        tags$head(tags$style(HTML(
          '
          thead, th {
            text-align: center;
            color: #FFF;
          }
          
         /* Jistify center title of table
          .justify-content-between {
              justify-content: center;
          }

         /*
          img {
            border-radius: 10px;
          }

          h3 {
            float: left;
            font-size: 1.1rem;
            font-weight: 400;
            margin: 0;
            color: #b90e13;
            text-transform: uppercase;
            text-shadow: 1px 1px 2px rgba(150, 150, 150, 1);
           }
          '
        ))),
        # fluidRow(
        #   column(2),
        #   column(8, h2("Registro historico de Tiburones de la Guaira", 
        #                style = "color: #707070; text-align: center; 
        #                         font-size: 1.6rem; font-family: 'Roboto Regular', sans-serif;
        #                         font-weight: 500; text-transform: uppercase;",
        #                align = 'center'),
        #          style = "padding-left: 20px;"),
        #   column(2)
        #   ),
        tabItems(
        # TabItem Home ----
          tabItem(
            tabName = 'inicio',
            tags$h2("BIENVENIDO A 60 AÑOS DE ESTADISTICAS DE TIBURONES DE LA GUAIRA", align = 'center'),
            fluidRow(
              column(7),
              column(5,
                     tags$head(
                       tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)  
                                 [0],p=/^http:/.test(d.location)?\'http\':\'https\';
                                 if(!d.getElementById(id)){js=d.createElement(s);
                                 js.id=id;js.src=p+"://platform.twitter.com/widgets.js";
                                 fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')
                     ),
                     tabsetPanel(
                       id = "tabset122",
                       side = "left",
                       # TibuStats ----
                       tabPanel(
                         tabName = 'TibuStats',
                           a("TibuStats en Twitter",
                             class = "twitter-timeline", 
                             href = "https://twitter.com/tibu_stats" 
                           )
                       ),
                     # Tiburones BBC ----
                       tabPanel(
                         tabName = 'Tiburones BBC',
                           a("Tiburones BBC en Twitter",
                             class = "twitter-timeline", 
                             href = "https://twitter.com/tiburones_net" 
                           )

                       )
                     )
                  )
                )
            # img(src = 'logo_60_anos.jpeg')
            ),
        # TabItem by Team ----
        tabItem(
          tabName = 'equipo',
          bs4TabSetPanel(
            id = "tabset",
            side = "left",
            tabPanel(
            # Picheo ----
              tabName = 'Picheo',
            br(),
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Temporada Regular",
                         DT::dataTableOutput('Preseason_team')
                         )
                       )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin", 
                         DT::dataTableOutput('Prr_team')
                         )
                      )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Final",
                         DT::dataTableOutput('Pfinal_team')
                         )
                      )
                )
              ),
            # Bateo ----  
            tabPanel(
              tabName = 'Bateo',
              br(),
              fluidRow(
                       column(12,
                              bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Temporada Regular",
                                DT::dataTableOutput('Breseason_team')
                                )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Round Robin",
                                DT::dataTableOutput('Brr_team')
                              )
                            )
                     ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Final",
                                DT::dataTableOutput('Bfinal_team')
                              )
                           )
                        )
                      )
            )
          ),
        # TabItem by Season ----
        tabItem(
          h4('Datos historicos por temporada', align = "center"),
          tabName = 'temporada',
          tabsetPanel(
            id = "tabset1",
            side = "left",
            # TabPanel Picheo ----
            tabPanel(
              tabName = 'Picheo', 
                     # Input ----
                    br(),
                     fluidRow(
                       br(),
                       column(2,
                              selectInput(
                                inputId = 'select_temporada',
                                label = 'Temporadas',
                                choices = temporadas
                              )
                       )
                     ),
                     # Tables Picheo ----
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Temporada Regular",
                                DT::dataTableOutput('picheo_rs')
                                )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Round Robin",
                                DT::dataTableOutput('picheo_rr_sm')
                                )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Dash::bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Final",
                                DT::dataTableOutput('picheo_finals')
                                )
                              )
                       )
                       ),
            # TabPanel Bateo ----
            tabPanel(
              tabName = 'Bateo',
              br(),
                     # Input ----
                     fluidRow(
                       br(),
                       column(2,
                              selectInput(
                                inputId = 'select_temporada_bat',
                                label = 'Temporadas',
                                choices = temporadas
                              )
                       )
                     ),
                     # Tables Bateo ----
                     fluidRow(
                       column(12,
                              bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Temporada Regular",
                                DT::dataTableOutput('bateo_rs')
                                )
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              bs4Card(
                                closable = FALSE,
                                width = NULL,
                                title = "Raound Robin",
                                DT::dataTableOutput('bateo_rr_sm')
                            )
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,
                            bs4Card(
                              closable = FALSE,
                              width = NULL,
                              title = "Final",
                              DT::dataTableOutput('bateo_finals')
                                )
                         )
                       )
              )
            # TabPanel Fildeo ----
              # tabPanel(tabName = 'Fildeo', tableOutput('fildeo_rs'))
            )
          ),
        # TabItem by Player ----
        tabItem(
          tabName = 'jugador',
          tabsetPanel(
            id = "tabset2",
            side = "left",
            # Picheo ----
            tabPanel(
              h2("Estadisticas historicas de lanzadores", 
                 style = 'color: #b90e13;
                         font-size: 19px;
                         font-weight: 400;
                         font-family: "Roboto Regular", sans-serif;
                         text-align: center;
                         text-transform: uppercase;
                         text-shadow: 1px 1px 2px rgba(150, 150, 150, 1);'),
              tabName = 'Picheo',
              br(),
              fluidRow(
                column(9),
                column(3,
                       selectInput(
                         inputId = 'select_jugador_pit',
                         label = 'Lanzadores',
                         choices = .pitchers,
                         selected = "Junior Guerra"
                            
                         )
                       )
                ),
              fluidRow(
                # Player name ----
                # column(1),
                column(2, imageOutput('jugador_pit', height = "100px", )),
                column(4, 
                       br(),
                       h2(textOutput("first_name_pit"),
                          style = "font-size: 1.5em;
                                   font-weight: 700;
                                   font-family: -apple-system,BlinkMacSystemFont,Roboto,Arial,Helvetica Neue,Helvetica,sans-serif;"),
                       h6(textOutput("POS_L")),
                       h6(textOutput("EXP_L")),
                       ),
                column(6)
              ),
              br(),
              # Tabpanel stats and info ----
              tabsetPanel(
                id = "tabset2",
                side = "left",
                # Carrera ----
                tabPanel(
                  active = TRUE,
                  tabName = 'Carrera',
                  fluidRow(
                    br(),
                    column(12,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Temporada Regular",
                             DT::dataTableOutput('picheo_jugador')
                             ),
                           br(),
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Round Robin",
                             DT::dataTableOutput('picheo_jugador_rr')
                             ),
                           br(),
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Finales",
                             DT::dataTableOutput('picheo_jugador_final')
                             )
                           )
                    )
                  ),
                # Game logs ----
                tabPanel(
                  tabName = 'Game Log',
                  fluidRow(
                    column(2,
                           selectInput(
                             inputId = 'select_temporada_pitlog',
                             label = 'Temporada',
                             choices = .pitching_log
                             )
                           )
                  ),
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Pitching Log",
                             DT::dataTableOutput('pit_log')
                             )
                           ),
                    column(4)
                  )
                 ),
                # Splits ---- 
                tabPanel(
                  tabName = 'Splits',
                  fluidRow(
                    column(2,
                           selectInput(
                             inputId = 'select_temporada_split',
                             label = 'Temporada',
                             choices = .batting_log
                             )
                           )
                    ),
                  # Day/Night ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "DIA/NOCHE",
                             DT::dataTableOutput('split_horario_pit')
                           )
                    ),
                    column(2)
                    ),
                  # Opponent ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "OPONENTE",
                             DT::dataTableOutput('split_opponent_pit')
                           )
                    ),
                    column(2)
                    ),
                  # Stadium ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "ESTADIO",
                             DT::dataTableOutput('split_stadium_pit')
                           )
                    ),
                    column(2)
                    ),
                  # Month ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "MES",
                             DT::dataTableOutput('split_month_pit')
                           )
                    ),
                    column(2)
                    ),
                  # Home Away ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "EN CASA/VISITANTE",
                             DT::dataTableOutput('split_homeaway_pit')
                           )
                    ),
                    column(2)
                    ),
                  # Pitcher Type ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "TIPO DE PITCHER",
                             DT::dataTableOutput('split_pitchertype_pit')
                           )
                    ),
                    column(2)
                    )
                  )
              ),
              hr()
              ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              h2("Estadisticas historicas de bateadores", 
                 style = 'color: #b90e13;
                         font-size: 19px;
                         font-weight: 400;
                         font-family: "Roboto Regular", sans-serif;
                         text-align: center;
                         text-transform: uppercase;
                         text-shadow: 1px 1px 2px rgba(150, 150, 150, 1);'),
              br(),
              fluidRow(
                column(9),
                column(3,
                       selectInput(
                         inputId = 'select_jugador_bat',
                         label = 'Bateadores',
                         choices = .bateadores,
                         selected = "Lorenzo Cedrola"
                         )
                       )
                ),
              fluidRow(
                # Player name ----
                # column(1),
                column(2, imageOutput('jugador_bat', height = "100%", )),
                column(4, 
                       br(),
                       h2(textOutput("first_name_bat"),
                          style = "font-size: 1.5em;
                                   font-weight: 700;
                                   font-family: -apple-system,BlinkMacSystemFont,Roboto,Arial,Helvetica Neue,Helvetica,sans-serif;"),
                       h6(textOutput("POS_B")),
                       h6(textOutput("EXP_B"))
                ),
                column(6)
              ),
              br(),
              # Tabpanel stats and info ----
              tabsetPanel(
                id = "tabset222",
                side = "left",
                # Carrera ----
                tabPanel(
                  active = TRUE,
                  tabName = 'Carrera',
                  fluidRow(
                    br(),
                    column(12,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Temporada Regular",
                             DT::dataTableOutput('bat_rs')
                           ),
                           br(),
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Round Robin",
                             DT::dataTableOutput('bat_rr')
                           ),
                           br(),
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "Finales",
                             DT::dataTableOutput('bat_final')
                             ) 
                           )
                    )
                  ),
                # Game Logs -----
                tabPanel(
                  tabName = 'Game Log',
                  fluidRow(
                    column(2,
                           selectInput(
                             inputId = 'select_temporada_batlog',
                             label = 'Temporada',
                             choices = .batting_log
                           )
                    )
                  ),
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "BATEO LOG",
                             DT::dataTableOutput('bat_log')
                           )
                    ),
                    column(4)
                    )
                  ),
                # Splits ----
                tabPanel(
                  tabName = 'Splits',
                  fluidRow(
                    column(2,
                           selectInput(
                             inputId = 'select_temporada_split_bat',
                             label = 'Temporada',
                             choices = .pitching_log
                           )
                    )
                  ),
                  # Day/Night ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "DIA/NOCHE",
                             DT::dataTableOutput('split_horario_bat')
                           )
                    ),
                    column(2)
                  ),
                  # Opponent ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "OPONENTE",
                             DT::dataTableOutput('split_opponent_bat')
                           )
                    ),
                    column(2)
                  ),
                  # Stadium ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "ESTADIO",
                             DT::dataTableOutput('split_stadium_bat')
                           )
                    ),
                    column(2)
                  ),
                  # Month ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "MES",
                             DT::dataTableOutput('split_month_bat')
                           )
                    ),
                    column(2)
                  ),
                  # Home Away ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "EN CASA/VISITANTE",
                             DT::dataTableOutput('split_homeaway_bat')
                           )
                    ),
                    column(2)
                    ),
                  # Order Bat ----
                  fluidRow(
                    column(8,
                           br(),
                           bs4Card(
                             closable = FALSE,
                             width = NULL,
                             title = "ORDEN AL BATE",
                             DT::dataTableOutput('split_order_bat')
                           )
                    ),
                    column(2)
                    )
                  )
                ),
              )
            )
          ),
        # TabItem by Position ----
        bs4TabItem(
          h4('Datos historicos por posicion', align = 'center'),
          tabName = 'posicion',
            # Picheo ----
          tabsetPanel(
            id = "tabset22",
            side = "left",
            tabPanel(
              tabName = 'Picheo',
              br(),
              # input position ----
              fluidRow(
                br(),
                column(2,
                       selectInput(
                         inputId = 'select_posicion_pit',
                         label = 'Posiciones',
                         # choices = posiciones 
                         choices = "P"
                       )
                )
              ),
              # Stats by regular season ----
              fluidRow(
                  column(12,
                         bs4Card(
                           closable = FALSE,
                           width = NULL,
                           title = "Temporada Regular",
                           DT::dataTableOutput('info_position_rs_pit')
                           )
                         )
                  ),
              # Stats by round Robin ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Roun Robin",
                         DT::dataTableOutput('info_position_rr_pit')
                         )
                       )
                ),
              # Stats by finals ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Final",
                         DT::dataTableOutput('info_position_final_pit')
                         )
                       )
                )
              ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              br(),
              # input position ----
              fluidRow(
                br(),
                column(2,
                       selectInput(
                         inputId = 'select_posicion_bat',
                         label = 'Posiciones',
                         # choices = posiciones
                         choices = c("C", "CF", "1B", "RF", "IF",
                                     "2B", "SS", "LF", "CE", "3B", "BD", "BE"),
                         selected = "C"
                       )
                )
              ),
              # Stats by regular season ----
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Temporada Regular",
                         DT::dataTableOutput('info_position_bat')
                       )
                    )
                  ),
              # Stats by round robin ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin",
                         DT::dataTableOutput('info_position_rr_bat')
                         )
                       )
                ),
              # Stats by final ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Final",
                         DT::dataTableOutput('info_position_final_bat')
                         )
                       )
                )
              )
            )
          ),
        # TabItem by country ----
        tabItem(
          h4('Datos historicos por Paises', align = "center"),
          tabName = 'paises',
          tabsetPanel(
            id = "tabset10",
            side = "left",
            # TabPanel Picheo ----
            tabPanel(
              tabName = 'Picheo', 
              br(),
              # Input ----
              fluidRow(
                br(),
                column(2,
                       selectInput(
                         inputId = 'select_country',
                         label = 'Paises',
                         choices = .paises_pitching
                         )
                       )
              ),
              # Tables Picheo ----
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Temporada Regular",
                         DT::dataTableOutput('picheo_rs_country')
                         )
                       )
                ),
                br(),
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin",
                         DT::dataTableOutput('picheo_rr_country')
                         )
                       )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Finales",
                         DT::dataTableOutput('picheo_finals_country')
                         )
                       )
                ),
              br(), 
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "Estadisticas Globales",
                         DT::dataTableOutput('country_pit')
                       )
                )
              )
            ),
            # TabPanel Bateo ----
            tabPanel(
              tabName = 'Bateo',
              br(),
              # Input ----
              fluidRow(
                br(),
                column(2,
                       selectInput(
                         inputId = 'select_country_bat',
                         label = 'Paises',
                         choices = .paises_batting
                       )
                )
              ),
              # Tables Bateo ----
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Temporada Regular",
                         DT::dataTableOutput('bateo_rs_country')
                         )
                       )
                ),
                br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin",
                         DT::dataTableOutput('bateo_rr_country')
                         )
                       )
                ),
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Finales",
                         DT::dataTableOutput('bateo_finals_country')
                         )
                       )
                ),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "Estadisticas Globales",
                         DT::dataTableOutput('country_bat')
                       )
                )
              ),
              br()
            )
            # TabPanel Fildeo ----
            # tabPanel(tabName = 'Fildeo', tableOutput('fildeo_rs'))
          )
        ),
        # TabItem by Roster ----
        bs4TabItem(
          h4('Rosters historicos por posicion', align = 'center'),
          tabName = 'roster',
          br(),
          # input roster ----
          fluidRow(
            br(),
            column(1),
            column(3,
                   selectInput(
                     inputId = 'select_rosters',
                     label = 'Temporadas',
                     choices = as.character(c("Todas las Temporadas", 
                                              temporadas)),
                     selected = "2021-22"
                     # multiple = TRUE
                     )
                   ),
            column(3,
                   selectInput(
                     inputId = 'countrys',
                     label = 'Paises',
                     choices = c("Todos los Paises", .paises)
                     )
                   ),
            column(3,
                   selectInput(
                     inputId = 'posiciones',
                     label = 'Posiciones',
                     choices = c("Todas las Posiciones", posiciones)
                     )
                   ),
            column(2)
          ),
          # Stats by roster ----
          br(),
          fluidRow(
            column(1),
            column(10,
                   bs4Card(
                     closable = FALSE,
                     width = NULL,
                     title = "Temporada Regular",
                     DT::dataTableOutput('info_roster')
                     )
                   ),
            column(1)
          )
        ),
        # Tabitem Estadísticas Vzla vs importados ----
        tabItem(
          h4('Venezolanos vs Importados', align = "center"),
          tabName = 'vzla',
          tabsetPanel(
            id = "tabpanel11",
            side = "left",
            # Picheo ----
            tabPanel(
              tabName = 'Picheo',
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Totales en Temporada Regular",
                         DT::dataTableOutput('versus_total_pit')
                       )
                )
              ),
              # Regular season ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Temporada Regular",
                         DT::dataTableOutput('versus_pit')
                       )
                )
              ),
              # Round Robin ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin",
                         DT::dataTableOutput('versus_rr_pit')
                       )
                )
              ),
              # Final ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Final",
                         DT::dataTableOutput('versus_final_pit')
                       )
                )
              )
            ),
            # Bateo ----
            tabPanel(
              tabName  = 'Bateo',
              br(),
                fluidRow(
                  column(12,
                         bs4Card(
                           closable = FALSE,
                           width = NULL,
                           title = "Totales en Temporada Regular",
                           DT::dataTableOutput('versus_total_bat')
                         )
                  )
                ),
              # Regular Season ----
                br(),
                fluidRow(
                  column(12,
                         bs4Card(
                           closable = FALSE,
                           width = NULL,
                           title = "Temporada Regular",
                           DT::dataTableOutput('versus_bat')
                         )
                  )
                ),
              # Round Robin ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Round Robin",
                         DT::dataTableOutput('versus_rr_bat')
                       )
                )
              ),
              # Regular Season ----
              br(),
              fluidRow(
                column(12,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         title = "Final",
                         DT::dataTableOutput('versus_final_bat')
                         )
                       )
                )
              )
            )
        ),
        # Tabitem Records ----
        tabItem(
          tabName = 'deporvida',
          h4('Records de por vida', align = 'center'),
          tabsetPanel(
            id = "tabpanel4",
            side = "left",
            tabPanel(
              # Picheo ----
              tabName = 'Picheo',
              #1 ----
              br(),
              fluidRow(
                column(4,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "W",
                         DT::dataTableOutput('p_w')
                         )
                       ),
                column(4,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "L",
                         DT::dataTableOutput('p_l')
                         )
                       ),
                column(4,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "G",
                         DT::dataTableOutput('p_g')
                         )
                       )
                ),
              br(),
              #2 ----
              fluidRow(
                column(4,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "GS",
                         DT::dataTableOutput('p_gs')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "IP",
                         DT::dataTableOutput('p_ip')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "SO",
                         DT::dataTableOutput('p_so')
                         )
                       )
              ),
              br(),
              #3 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "H",
                         DT::dataTableOutput('p_h')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "BB",
                         DT::dataTableOutput('p_bb')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SV",
                         DT::dataTableOutput('p_sv')
                         )
                       )
              ),
              br(),
              #4 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "WHIP",
                         DT::dataTableOutput('p_whip')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "ERA",
                         DT::dataTableOutput('p_era')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "SO/BB",
                         DT::dataTableOutput('p_sobb')
                       )
                )
              ),
              br(),
              #5 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "H/9",
                         DT::dataTableOutput('p_h9')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SO/9",
                         DT::dataTableOutput('p_so9')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "BB/9",
                         DT::dataTableOutput('p_bb9')
                         )
                       )
                )
              ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              #1 ----
              br(),
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "H",
                         DT::dataTableOutput('b_hits')
                          )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "2B",
                         DT::dataTableOutput('b_2b')
                          )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "3B",
                         DT::dataTableOutput('b_3b')
                          )
                       )
              ),
              br(),
              #2 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "HR",
                         DT::dataTableOutput('b_hr')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "AVG",
                         DT::dataTableOutput('b_average')
                          )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "RBI",
                         DT::dataTableOutput('b_rbi')
                         )
                       )
                  ),
              br(),
              #3 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SLG",
                         DT::dataTableOutput('b_slg')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "OBP",
                         DT::dataTableOutput('b_obp')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "OPS",
                         DT::dataTableOutput('b_ops')
                         )
                       )
                  ),
              br(),
              #4 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "AB",
                         DT::dataTableOutput('b_ab')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SB",
                         DT::dataTableOutput('b_sb')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "XB",
                         DT::dataTableOutput('b_xb')
                         )
                       )
                )
              )
            )
          ),
        # Records by season ----
        tabItem(
          tabName = 'por_temporadas',
          h4('Records por temporadas ', align = 'center'),
          tabsetPanel(
            id = "tabpanel6",
            side = "left",
            tabPanel(
            # Picheo ----
              tabName = 'Picheo',
              #1 ----
              br(),
              fluidRow(
                column(4,
                       bs4Dash::bs4Card(
                         solidHeader = FALSE, 
                         collapsible = TRUE,
                         width = NULL,
                         higth = '300px',
                         closable = FALSE,
                         title = "W",
                         DT::dataTableOutput('pt_p_w')
                         )
                       ),
                column(4,
                       bs4Dash::bs4Card(
                         solidHeader = FALSE, 
                         collapsible = TRUE,
                         # labelText = 1,
                         # labelStatus = "danger",
                         # labelTooltip = "Hi Bro!",
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         title = "L",
                         DT::dataTableOutput('pt_p_l')
                         )
                       ),
                column(4,
                       bs4Dash::bs4Card(
                         solidHeader = FALSE, 
                         collapsible = TRUE,
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         title = "G",
                         DT::dataTableOutput('pt_p_g')
                         )
                       )
                ),
              br(),
              #2 ----
              fluidRow(
                column(4,
                       bs4Dash::bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "GS",
                         DT::dataTableOutput('pt_p_gs')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "IP",
                         DT::dataTableOutput('pt_p_ip')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "SO",
                         DT::dataTableOutput('pt_p_so')
                         )
                       )
                ),
              br(),
              #3 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "H",
                         DT::dataTableOutput('pt_p_h')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "BB",
                         DT::dataTableOutput('pt_p_bb')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SV",
                         DT::dataTableOutput('pt_p_sv')
                         )
                       )
                ),
              br(),
              #5 ------
            fluidRow(
              column(4,
                     bs4Card(
                       closable = FALSE,
                       width = NULL,
                       higth = '100px',
                       collapsible = TRUE,
                       title = "H/9",
                       DT::dataTableOutput('pt_p_h9')
                     )
              ),
              column(4,
                     bs4Card(
                       closable = FALSE,
                       width = NULL,
                       higth = '300px',
                       collapsible = TRUE,
                       # status = 'warning',
                       title = "SO/9",
                       DT::dataTableOutput('pt_p_so9')
                     )
              ),
              column(4,
                     bs4Card(
                       closable = FALSE,
                       width = NULL,
                       higth = '300px',
                       collapsible = TRUE,
                       # status = 'warning',
                       title = "BB/9",
                       DT::dataTableOutput('pt_p_bb9')
                       )
                     )
              )
            ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              #1 ----
              br(),
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "H",
                         DT::dataTableOutput('pt_b_hits')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "2B",
                         DT::dataTableOutput('pt_b_2b')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "3B",
                         DT::dataTableOutput('pt_b_3b')
                         )
                       )
              ),
              br(),
              #2 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "HR",
                         DT::dataTableOutput('pt_b_hr')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "AVG",
                         DT::dataTableOutput('pt_b_average')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SLG",
                         DT::dataTableOutput('pt_b_slg')
                         )
                       )
              ),
              br(),
              #3 ----
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "OBP",
                         DT::dataTableOutput('pt_b_obp')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "OPS",
                         DT::dataTableOutput('pt_b_ops')
                       )
                ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "RBI",
                         DT::dataTableOutput('pt_b_rbi')
                         )
                       )
                ),
              #4 ----
              br(),
              fluidRow(
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '100px',
                         collapsible = TRUE,
                         title = "AB",
                         DT::dataTableOutput('pt_b_ab')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "SB",
                         DT::dataTableOutput('pt_b_sb')
                         )
                       ),
                column(4,
                       bs4Card(
                         closable = FALSE,
                         width = NULL,
                         higth = '300px',
                         collapsible = TRUE,
                         # status = 'warning',
                         title = "XB",
                         DT::dataTableOutput('pt_b_xb')
                         )
                       )
                )
              )
            )
          ),
        # Records LVBP ----
        tabItem(
          tabName = 'lvbp',
          h2('Records en la LVBP ', align = 'center'),
          br(),
          fluidRow(
            column(2),
            column(8,
                  bs4Dash::bs4Box(
                    width = 12,
                    higth = '1900px',
                    collapsible = TRUE,
                    title = " ",
                    DT::dataTableOutput('lvbp_general')
                    )
                  ),
            column(2)
              )
          ),
        # Retirados ----
        tabItem(
          tabName = 'retirados',
          h4('Números retirados', align = 'center'),
          br(),
          fluidRow(
            column(1),
            column(10,
                   bs4Dash::bs4Box(
                     # height = "1200px",
                     status = "success",
                     width = 12,
                       # title = "Premios en la LVBP",
                     #1 ----
                     userList(
                       userListItem(
                         image = "luissalazar3.png",
                         title = h5("Luis Salazar", style = "font-size': '14px'"),
                         subtitle = h6("SS - Retirado en 1993", style = "font-size': '12px'")
                       ),
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2014/04/knitr.png",
                         title = h5("Ángel Bravo", style = "font-size': '14px'"),
                         subtitle = h6("OF - Retirado en 1978", style = "font-size': '12px'")
                         ),
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2017/05/rmarkdown.png",
                         title = h5("Luis Aparicio", style = "font-size': '14px'"),
                         subtitle = h6("SS - Retirado en 1968", style = "font-size': '12px'")
                         ),
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2014/04/tidyr.png",
                         title = h5('Carlos "Café" Martínez', style = "font-size': '14px'"),
                         subtitle = h6("OF - Retirado en 1997", style = "font-size': '12px'"),
                        )
                       ),
                     #2 ----
                     hr(),
                     userList(
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2014/04/tidyr.png",
                         title = h5("Gustavo Polidor", style = "font-size': '14px'"),
                         subtitle = h6("SS - Retirado en 1995", style = "font-size': '12px'")
                         ),
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2014/04/packrat.png",
                         title = h5("Alfredo Pedrique", style = "font-size': '14px'"),
                         subtitle = h6("IF - Retirado en 2003", style = "font-size': '12px'")
                         ),
                       userListItem(
                         image = "https://www.rstudio.com/wp-content/uploads/2014/04/sparklyr.png",
                         title = h5("Raúl Pérez Tovar", style = "font-size': '14px'"),
                         subtitle = h6("OF - Retirado en 2020", style = "font-size': '12px'")
                         ),
                       userListItem(
                         image = "https://d33wubrfki0l68.cloudfront.net/071952491ec4a6a532a3f70ecfa2507af4d341f9/c167c/images/hex-dplyr.png",
                         title = h5("Oswaldo Guillén", style = "font-size': '14px'"),
                         subtitle = h6("SS - Retirado en 2000", style = "font-size': '12px'")
                        )
                       ),
                     #3 ----
                       hr(),
                       userList(
                         userListItem(
                           image = "https://www.rstudio.com/wp-content/uploads/2014/04/sparklyr.png",
                           title = h5("Aurelio Monteagudo", style = "font-size': '14px'"),
                           subtitle = h6("P - Retirado en 1982", style = "font-size': '12px'")
                           )
                         )
                     )
                  ),
            column(2)
              )
          ),
        # Stats geographics ----
        tabItem(
          tabName = 'geograficas',
          h4('Dashboard de estadísticas geográficas', align = 'center'),
          tabsetPanel(
            id = "tabset8",
            side = "left",
            # General ----
            tabPanel(
              tabName = 'General',
              br(),
              fluidRow(
                column(8,
                       # highchartOutput('country_chart', width = '800px', height = '800px')
                       highchartOutput('country_chart', width = '800px', height = '800px')
                       # highchartOutput('country_chart')
                       ),
                # column(2,
                #        bs4ValueBoxOutput("valuebox_cpit", width = 12),
                #        bs4ValueBoxOutput("valuebox_lan", width = 12),
                #        bs4ValueBoxOutput("valuebox_right", width = 12),
                #        bs4ValueBoxOutput("valuebox_left", width = 12)
                #        ),
                column(4,
                       highchartOutput('foreign_chart'),
                       highchartOutput('position_chart')
                       )
                )
              ),
            # Por paises -----
            tabPanel(
              tabName = 'Por Paises',
              br(),
              fluidRow(
                column(8,
                       highchartOutput('paises_chart', width = '800px', height = '800px')
                ),
                # column(2,
                #        bs4ValueBoxOutput("valuebox_cpit", width = 12),
                #        bs4ValueBoxOutput("valuebox_lan", width = 12),
                #        bs4ValueBoxOutput("valuebox_right", width = 12),
                #        bs4ValueBoxOutput("valuebox_left", width = 12)
                #        ),
                column(4,
                       highchartOutput('foreign_chart_paises'),
                       highchartOutput('position_chart_paises')
                  )
                )
              )
            )
          ),
        # Glosary ----
        tabItem(
          tabName = "g_stat",
          # First row ----
          fluidRow(
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Wins (W)")),
                   tags$div(class = "footer-glossary-text", "Juegos ganados")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Losses (L)")),
                   tags$div(class = "footer-glossary-text", "Juegos perdidos")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Games Pitched (G)")),
                   tags$div(class = "footer-glossary-text", "Juegos lanzados")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Game Started (GS)")),
                   tags$div(class = "footer-glossary-text", "Juegos perdidos")
                   )
            ),
          br(),
          br(),
          # Second row ----
          fluidRow(
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Game Complete (GC)")),
                   tags$div(class = "footer-glossary-text", "Juegos ganados")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Shutout (SHO)")),
                   tags$div(class = "footer-glossary-text", "Juegos completo sin permitir carreras")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Saves (SV)")),
                   tags$div(class = "footer-glossary-text", "Juegos salvados")
                   ),
            column(3,
                   tags$div(class = "footer-glossary-title", tags$a("Hits Allowed (H)")),
                   tags$div(class = "footer-glossary-text", "Sencillos permitidos")
                   )
            )
        ),
        # Advanced Search ----
        tabItem(
          tabName = 'busqueda',
          tabsetPanel(
            id = "tabset18",
            side = "left",
            # Picheo ----
            tabPanel(
              tabName = 'Picheo',
              br(),
              fluidRow(
                box(
                  title = h2('Parametros de busqueda', align = "center"),
                  width = 12,
                  collapsible = FALSE,
                  closable = FALSE,
                  fluidRow(
                    # 1 ----
                    column(2,
                           pickerInput(
                           inputId = "ronda",
                           label = "Ronda:", 
                           choices = c("regular" , "Round Robin", "Finales"),
                           selected = "Finales"
                         ),
                         pickerInput(
                           inputId = "desde_p",
                           label = "Desde:", 
                           # choices = attr(UScitiesD, "Labels"),
                           choices = temporadas_asc,
                           options = list(
                             `live-search` = TRUE)
                         ),
                         pickerInput(
                           inputId = "hasta_p",
                           label = "Hasta:", 
                           choices = temporadas,
                           options = list(
                             `live-search` = TRUE)
                         )
                       ),
                    # 2 ----
                column(4,
                       fluidRow(
                         column(
                           width = 12,
                           pickerInput(
                             inputId = "Id049",
                             label = "País de Nacimiento:", 
                             choices = c("Nacido en", "No Nacido en"),
                             selected = "Nacido en"
                           ),
                           pickerInput(
                             inputId = "Id080",
                             label = "País:",
                             # choices = c("Venezuela", "USA", "Cuba", "Republica Dominicana"),
                             choices = .paises_pitching,
                             inline = FALSE,
                             options = list(
                               `live-search` = TRUE)
                             ),
                           pickerInput(
                             inputId = "Id086",
                             label = "Lanza", 
                             choices = c("Derecha", "Izquierda", "Indistinto"),
                             selected = "Indistinto",
                             options = list(
                               `live-search` = TRUE)
                             )
                           )
                         )
                       ),
                    # 3 ----
                column(4,
                       # 1 criteria ----
                       fluidRow(
                         column(4,
                             pickerInput(
                               inputId = "Id086",
                               label = "Criterio 1:", 
                               # choices = c("W" = "w", "L" = "l", "GS" = "gs", "SO" = "so", "WHIP" = "whip"),
                               choices = c("w", "l", "gs", "so", "whip"),
                               selected = "W",
                               options = list(
                                 `live-search` = TRUE)
                               )
                             ),
                           column(3,
                                  pickerInput(
                                    inputId = "Id076",
                                    label = " ",
                                    choices = c(">=", "<=", "="),
                                    selected = "W",
                                    options = list(
                                      `live-search` = TRUE)
                                    )
                                  ),
                           column(3,
                                  textInput(
                                    "criteria_1",
                                    label = " "
                                    ) 
                                  )
                           ),
                       # 2 Criteria ----
                       fluidRow(
                         column(4,
                                pickerInput(
                                  inputId = "Id086",
                                  label = "Criterio 2:", 
                                  choices = c("W", "L", "GS", "SO", "WHIP"),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                pickerInput(
                                  inputId = "Id086",
                                  label = " ",
                                  choices = c(">=", "<=", "="),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                textInput(
                                  "criteria_2", 
                                  " ", 
                                  value = " "
                                  ) 
                                )
                         ),
                       # 3 Criteria ----
                       fluidRow(
                         column(4,
                                pickerInput(
                                  inputId = "Id086",
                                  label = "Criterio 3:", 
                                  choices = c("W", "L", "GS", "SO", "WHIP"),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                pickerInput(
                                  inputId = "Id086",
                                  label = " ",
                                  choices = c(">=", "<=", "="),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                textInput(
                                  "criteria_3", 
                                  " ", 
                                  value = " "
                                  ) 
                           )
                         ),
                       # 4 criteria ----
                       fluidRow(
                         column(4,
                                pickerInput(
                                  inputId = "Id086",
                                  label = "Criterio 4:", 
                                  choices = c("W", "L", "GS", "SO", "WHIP"),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                pickerInput(
                                  inputId = "Id086",
                                  label = " ",
                                  choices = c(">=", "<=", "="),
                                  selected = "W",
                                  options = list(
                                    `live-search` = TRUE)
                                )
                         ),
                         column(3,
                                textInput(
                                  "criteria_4", 
                                  " ", 
                                  value = " "
                                  ) 
                           )
                         )
                       )
                    ),
                # Search Icon ----
                fluidRow(
                  column(8,
                         fluidRow(
                           column(2),
                           column(8,
                                  actionBttn(
                                    inputId = "btn_searh_pit",
                                    label = "Buscar", 
                                    style = "material-flat",
                                    color = "primary",
                                    icon = icon("search-plus"),
                                    block = FALSE
                                  )
                           ),
                           column(2)
                           )
                         )
                    )
                  )
                ),
              br(),
              br(),
              # Final table search ----
              fluidRow(
                column(12,
                       bs4Dash::bs4Box(
                         width = 12,
                         # higth = '1900px',
                         collapsible = TRUE,
                         title = "Busqueda Avanzada",
                         DT::dataTableOutput('advance_pitching')
                         )
                       )
                )
              ),
            # Bateo ----
            tabPanel(
              tabName = 'Bateo',
              br()
              )
            )
          )
        )
        # End body ----
      )
    ),
    
    # Server ----
    server = function(input, output, session) {
     
      # Reactive Rosters ----
      Rosters <- reactive({
        .Rosters <- read_csv('data/rosters_clean.csv')
        
        .rosters <- .Rosters %>%
          arrange(jugador, years) %>%
          distinct(name) %>%
          mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
          )
        
        Rosters <- .Rosters %>%
          arrange(jugador, years) %>%
          left_join(.rosters, by = 'name') %>% 
          mutate(
            key = paste0(as.character(years), jugador),
            f_nac = as.Date(f_nac, format = "%m/%d/%Y")
            )
        
      })
      
      # Reactive Batting ----
      brs <- reactive({
        brs <- readr::read_csv("data/batting_reseason.csv", 
                               col_types = cols(pa = col_double(), cs = col_double(), 
                                                bb = col_number(), so = col_double(), 
                                                obp = col_double(), ir = col_double(), 
                                                rc = col_double(), hbp = col_double(), 
                                                sh = col_double(), sf = col_double())
                               )
      })
      
      # Reactive Pitching ----
      prs <- reactive({
        prs <- read_csv('data/pitching_reseason.csv')
      })
      
      # Reactive info player ----
      info_player <- reactive({
        req(input$select_jugador)
        
        df <- Rosters %>% 
          filter(jugador %in% input$select_jugador) 
        
      })
      # Reactive Record LVBP ----
      lvbp <- reactive({
        lvbp <- read_csv('data/records.csv')
      })
      
      
      # Reactive Geographic Stats ----
      # Reactive Pitching Logs ----
      Pitlog <- reactive({
        pitlog <- read_csv('data/pitching_log.csv') %>% 
          janitor::clean_names() %>% 
          dplyr::rename(years = temporada) %>% 
          dplyr::select(-jugador) %>% 
          dplyr::left_join(Rosters() %>% 
                             dplyr::select(-ronda),
                           by = c("player_id", "years")) %>% 
          dplyr::mutate(oponente = 
                          dplyr::case_when(
                            oponente == "Tiburones de la Guaira" ~ "TIB",
                            oponente == "Caribes de Anzoategui" ~ "CAR",
                            oponente == "Cardenales de Lara" ~ "CARD",
                            oponente == "Aguilas del Zulia" ~ "AGU",
                            oponente == "Leones del Caracas" ~ "LEO",
                            oponente == "Bravos de Margarita" ~ "BRA",
                            oponente == "Navegantes del Magallanes" ~ "NAV",
                            oponente == "Tigres de Aragua" ~ "TIG")
          )
      })
      
      # Reactive Batting Logs ----
      Batlog <- reactive({
        batlog <- read_csv('data/batting_log.csv') %>% 
          janitor::clean_names() %>% 
          dplyr::rename(years = temporada) %>% 
          dplyr::select(-jugador) %>% 
          dplyr::left_join(Rosters() %>% 
                             dplyr::select(-ronda),
                           by = c("player_id", "years")) %>% 
          dplyr::mutate(oponente = 
                          dplyr::case_when(
                            oponente == "Tiburones de la Guaira" ~ "TIB",
                            oponente == "Caribes de Anzoategui" ~ "CAR",
                            oponente == "Cardenales de Lara" ~ "CARD",
                            oponente == "Aguilas del Zulia" ~ "AGU",
                            oponente == "Leones del Caracas" ~ "LEO",
                            oponente == "Bravos de Margarita" ~ "BRA",
                            oponente == "Navegantes del Magallanes" ~ "NAV",
                            oponente == "Tigres de Aragua" ~ "TIG")
          )
      })
      
      # Reactive Game results ----
      GameResult <- reactive({
        game_result <- read_csv('data/game_result.csv') %>% 
          janitor::clean_names() 
      })
      
      #By Pitching Log -----
      # Table pitching log ----
      output$pit_log <- DT::renderDataTable({

        # Data ----
        data <- Pitlog() %>%
          dplyr::mutate(player = paste0(first_name, " ", last_name)) %>% 
          dplyr::select(years, fecha, player,  n, fecha, ip, h, c, cl, hr, bb, so,bf, efe, sv, hld, g, p,
                        oponente, equipo, ronda) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_pitlog
            # player == "Junior Guerra",
          #   # years == "2021-22"
            ) %>%
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, oponente, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld) %>% 
          rename(
            `DATE` = fecha,
            `VS` = oponente,
            `IP` = ip,
            `ERA` = efe,
            `WHIP` = whip,
            `H` = h,
            `R` = c,
            `ER` = cl,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `BF` = bf,
            `G` = g,
            `P` = p,
            `SV` = sv,
            `HLD` = hld
            ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          data,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:15))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

      })
      
      # Table batting log ----
      output$bat_log <- DT::renderDataTable({

        # Data ----
        data <- Batlog() %>%
        # data <- batlog %>%
          dplyr::mutate(player = paste0(first_name, " ", last_name)) %>% 
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat
          ) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_bat,
            years == input$select_temporada_batlog
            # player == "Lorenzo Cedrola",
          #   # years == "2021-22"
            ) %>%
          mutate(
            avg = round((h) / ab, 3)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, oponente, ab, r, h, x2b, x3b, hr, bb, so, avg, ) %>% 
          rename(
            `DATE` = fecha,
            `VS` = oponente,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = x2b,
            `3B` = x3b,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg
            ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          data,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 30,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:10))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

      })
      
      #By Splits ----
      # Table picheo Splits by Day/Night ----
      output$split_horario_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>% 
          left_join(GameResult() %>% 
                      select(-fecha), by = "juego") %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
        filter(
          ronda == "regular",
          equipo == "Tiburones de la Guaira",
          player == input$select_jugador_pit,
          years == input$select_temporada_split
          # jugador == "J. Guerra",
          #   # years == "2021-22"
        ) %>%
        mutate(whip = round((bb + h)/ ip, 2)) %>% 
        arrange(n, desc(fecha)) %>% 
        select(fecha, oponente, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld, horario) %>% 
          group_by(horario) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 3),
            # BA = sum(H, na.rm = TRUE)
            .groups = "drop"
          ) %>% 
        rename(
          `CATEGORY` = horario
        ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
          
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
         df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:14))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      # Table picheo Splits by Opponet ----
      output$split_opponent_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>% 
          mutate(
            op = paste0("vs", " ", oponente),
            player = paste0(first_name, " ", last_name)) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_split
            # jugador == "J. Guerra",
          ) %>%
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, oponente, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, op, hld) %>% 
          group_by(op) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 2),
            .groups = "drop"
          ) %>% 
          rename(
            `CATEGORY` = op
          ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:14))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      # Table picheo by Stadium ----
      output$split_stadium_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>% 
          mutate(
            player = paste0(first_name, " ", last_name)
            ) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_split
            # jugador == "J. Guerra",
          ) %>%
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, estadio, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld) %>% 
          group_by(estadio) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 2),
            .groups = "drop"
          ) %>% 
          rename(
            `CATEGORY` = estadio
          ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(1:14)),
                              list(width = '200px', targets = 0)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      # Table picheo by Month ----
      output$split_month_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>% 
          mutate(
            player = paste0(first_name, " ", last_name),
            month = lubridate::month(fecha),
            month = case_when(
              month == 10 ~ "Octubre",
              month == 11 ~ "Noviembre",
              month == 12 ~ "Diciembre",
              month == 01 ~ "Enero",
              month == 1 ~ "Enero"
              )
            ) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_split
            # jugador == "J. Guerra",
          ) %>%
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, month, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld) %>% 
          group_by(month) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 2),
            .groups = "drop"
          ) %>% 
          rename(
            `CATEGORY` = month
          ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:14))
                              # list(width = '200px', targets = 0)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      # Table picheo by Homeaway ----
      output$split_homeaway_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>%
        # df <- pitlog %>% 
          left_join(GameResult(), by = "juego") %>% 
          mutate(
            player = paste0(first_name, " ", last_name),
            homeaway = case_when(
              home_club_equipo == "Tiburones de La Guaira" ~ "En Casa",
              TRUE ~ "Visitante"
            )
          ) %>% 
          filter(
            ronda == "regular",
            # equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_split
            # jugador == "J. Guerra"
          ) %>% 
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          select(homeaway, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld) %>% 
          group_by(homeaway) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 2),
            .groups = "drop"
          ) %>% 
          rename(
            `CATEGORY` = homeaway
          ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:14))
                              # list(width = '200px', targets = 0)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      # Table picheo by pitcher type ----
      output$split_pitchertype_pit <- DT::renderDataTable({
        
        # Data ----
        df <- Pitlog() %>%
        # df <- pitlog %>%
          left_join(GameResult() %>% dplyr::select(-fecha), by = "juego") %>% 
          mutate(
            player = paste0(first_name, " ", last_name),
            pitchertype = case_when(
              orden == 1 ~ "Abridor",
              orden != 1 ~ "Relevista",
            )
          ) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_pit,
            years == input$select_temporada_split
            # player == "Junior Guerra",
          ) %>%
          mutate(whip = round((bb + h)/ ip, 2)) %>% 
          arrange(n, desc(fecha)) %>% 
          select(fecha, pitchertype, ip, efe, whip, h, c, cl, hr, bb, so, bf, g, p, sv, hld) %>% 
          group_by(pitchertype) %>% 
          summarise(
            H = sum(h, na.rm = TRUE),
            R = sum(c, na.rm = TRUE),
            ER = sum(cl, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            BF = sum(bf, na.rm = TRUE),
            G = sum(g, na.rm = TRUE),
            P = sum(p, na.rm = TRUE),
            SV = sum(sv, na.rm = TRUE),
            HLD = sum(hld, na.rm = TRUE),
            IP = IP(ip),
            WHIP = round(sum(h, bb) / IP, 2),
            ERA = round((sum(ER, na.rm = TRUE) / IP) * 9, 2),
            .groups = "drop"
          ) %>% 
          rename(
            `CATEGORY` = pitchertype
          ) %>% 
          select(CATEGORY, 9:10, 13:15, 2:8,11:12)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:14))
                              # list(width = '200px', targets = 0)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      # Table bateo Splits by Day/Night ----
      output$split_horario_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
        # df <- batlog %>% 
          left_join(GameResult() %>% 
                      select(-fecha), by = "juego") %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat, horario
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(horario) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = horario) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:12))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      # Table bateo Splits by Opponet ----
      output$split_opponent_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
        # df <- batlog %>% 
          left_join(GameResult() %>% 
                      select(-fecha), by = "juego") %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat, oponente
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(oponente) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = oponente) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:12))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      # Table bateo by Stadium ----
      output$split_stadium_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
        # df <- batlog %>% 
          left_join(GameResult() %>% 
                      select(-fecha), by = "juego") %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat, estadio
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(estadio) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = estadio) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(1:12)),
                              list(width = '200px', targets = 0)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      # Table bateo by homeaway ----
      output$split_homeaway_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
          left_join(GameResult() %>% dplyr::select(-fecha), by = "juego") %>%
          mutate(
            player = paste0(first_name, " ", last_name),
            homeaway = case_when(
              home_club_equipo == "Tiburones de La Guaira" ~ "En Casa",
              TRUE ~ "Visitante"
            )
          ) %>% 
          filter(
            ronda == "regular",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat, homeaway
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(homeaway) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = homeaway) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:12))
                              # list(width = '100px', targets = 1)
            ),,
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      # Table bateo by bat order ----
      output$split_order_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
        # df <- batlog %>%
          left_join(GameResult() %>% dplyr::select(-fecha), by = "juego") %>%
          mutate(
            player = paste0(first_name, " ", last_name),
            ordenbat = case_when(
              orden_bat == 1 ~ "1ero",
              orden_bat == 2 ~ "2do",
              orden_bat == 3 ~ "3ero",
              orden_bat == 4 ~ "4to",
              orden_bat == 5 ~ "5to",
              orden_bat == 6 ~ "6to",
              orden_bat == 7 ~ "7mo",
              orden_bat == 8 ~ "8vo",
              orden_bat == 9 ~ "9no"
              )
            ) %>% 
          filter(
            ronda == "regular",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, ordenbat
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(ordenbat) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = ordenbat) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:12))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      # Table bateo by Month ----
      output$split_month_bat <- DT::renderDataTable({
        
        # Data ----
        df <- Batlog() %>%
          mutate(
            player = paste0(first_name, " ", last_name),
            month = lubridate::month(fecha),
            month = case_when(
              month == 10 ~ "Octubre",
              month == 11 ~ "Noviembre",
              month == 12 ~ "Diciembre",
              month == 01 ~ "Enero",
              month == 1 ~ "Enero"
            )
          ) %>% 
          filter(
            ronda == "regular",
            equipo == "Tiburones de la Guaira",
            player == input$select_jugador_bat,
            years == input$select_temporada_split_bat
            # player == "Lorenzo Cedrola",
              # years == "2021-22"
          ) %>%
          dplyr::select(
            years, fecha, player, ab, r, h, x2b, x3b, hr, bb, so, estadio ,oponente, equipo, ronda, n, orden_bat, month
          ) %>% 
          mutate( 
            avg = round((h) / ab, 3),
            tb = (h - x2b - x3b - hr) + (x2b * 2) + (x3b * 3) + (hr * 4),
            x1b = (h - x2b - x3b - hr)
            ) %>% 
          arrange(n, desc(fecha)) %>% 
          group_by(month) %>% 
          summarise(
            G = n(),
            AB = sum(ab, na.rm = TRUE),
            R = sum(r, na.rm = TRUE),
            H = sum(x1b, na.rm = TRUE),
            x2b = sum(x2b, na.rm = TRUE),
            x3b = sum(x3b, na.rm = TRUE),
            HR = sum(hr, na.rm = TRUE),
            BB = sum(bb, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            SO = sum(so, na.rm = TRUE),
            TB = sum(tb, na.rm = TRUE),
            TH = sum(h, na.rm = TRUE),
            AVG = round(TH / AB, 3),
            SLG = round((sum(x1b) + (x2b * 2) + ((x3b) * 3) + (HR * 4)) / AB, 3)
          ) %>% 
          rename(
            `2B` = x2b,
            `3B` = x3b,
          ) %>% 
          rename(`CATEGORY` = month) %>% 
          select(-TH)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            # pageLegth = 20,
            # lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:12))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      
      
      
      #By Team -----
      # Table pitching regular season by team ----
      output$Preseason_team <- DT::renderDataTable({
        
        # Data ----
        player_summarise <- prs() %>% 
          filter(ronda == "regular") %>% 
          arrange(years, jugador) %>% 
          select(-bk, -refuerzo, -ronda, -resultado) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad, na.rm = T), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
            ) 
        
        pitching_player <- prs() %>% 
          filter(ronda == "regular") %>% 
          arrange(years, jugador) %>% 
          select(-bk, -refuerzo, -ronda, -resultado) %>% 
          group_by(years) %>% 
          summarise(
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) %>% 
          ungroup() %>% 
          arrange(desc(years))
        
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) %>% 
          select(-G)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadísticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:21))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      # Table picheo round robin by team ----
      output$Prr_team <- DT::renderDataTable({
        
        # Data ----
        player_summarise <- prs() %>%
          filter(ronda == "round robin") %>% 
          arrange(years, jugador) %>% 
          select(-bk, -ronda, -resultado) %>% 
          summarise(
            years = 'Total',
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            ref = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = 'drop'
          )
        
        pitching_player <- prs() %>%
          filter(ronda == "round robin") %>% 
          arrange(years, jugador) %>% 
          select(-bk, -ronda, -resultado) %>%
          group_by(years) %>% 
          summarise(
            edad = as.character(round(mean(edad, na.rm = T), 1)),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            ref = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = "drop"
          ) %>% 
          arrange(desc(years))
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`,
            `REF` = `ref`
          ) %>% 
          select(-G)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        # footerCallback <- c(
        #   "function(tfoot, data, start, end, display){",
        #   "  $('th', tfoot).css('border-bottom', 'none');",
        #   "}"
        # )
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
                htmltools::em('Las estadísticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
          #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table picheo final by team ----
      output$Pfinal_team <- DT::renderDataTable({
        
        # Data ----
        player_summarise <- prs() %>% 
          filter(ronda == "finales") %>% 
          select(-bk) %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
         summarise(
            edad = round(mean(edad), 1),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so / bb, 2),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado),
            .groups = 'drop'
          ) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = sum(refuerzo, na.rm = T),
            resultado = sum(ifelse(resultado == 'Campeon', 1, 0)),
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>% 
          filter(ronda == "finales") %>% 
          arrange(years, jugador) %>% 
          select(-bk) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado)
          ) %>% 
          arrange(desc(years))
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            REF = refuerzo,
            RESULTADO = resultado,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
            ) %>% 
          select(-G)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        # footerCallback <- c(
        #   "function(tfoot, data, start, end, display){",
        #   "  $('th', tfoot).css('border-bottom', 'none');",
        #   "}"
        # )
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
          #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            pageLegth = 30,
            # lengthMenu = c(15, 20, 25),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:23))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table bateo regular season by team ----
      output$Breseason_team <- DT::renderDataTable({
        
        # Data ---- 
        player_summarise <- brs() %>% 
          filter(ronda == "regular") %>% 
          arrange(years, jugador) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 2),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) 
        
        
        batting_player <- brs() %>% 
          filter(ronda == "regular") %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        df <- rbind(batting_player, player_summarise) %>% 
          rename(
            Temporada = years,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:24))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      # Table bateo round robin by team ----
      output$Brr_team <- DT::renderDataTable({
        
        # Data ----
        player_summarise <- brs() %>% 
          filter(ronda == "round robin") %>% 
          arrange(years, jugador) %>% 
          summarise(
            years = 'Total',
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>% 
          filter(ronda == "round robin") %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        df <-  rbind(player_summarise, batting_player) %>%
          rename(
            `Temporada` = years,
            `Refuerzo` = refuerzo,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          )  
        
        # Table ----
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          class = 'display', # To remove lines horizontal in table
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 0:25)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      
      # Table bateo final by team ----
      output$Bfinal_team <- DT::renderDataTable({
        
        # Data ----
        player_summarise <-  brs() %>% 
          filter(ronda == "finales") %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado),
            .groups = 'drop'
          ) %>% 
          summarise(
            years = "Total",
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(refuerzo, na.rm = T),
            resultado = sum(ifelse(resultado == 'campeon', 1, 0)),
            .groups = 'drop'
          )
          
        
        batting_player <-  brs() %>% 
          filter(ronda == "finales") %>% 
          arrange(years, jugador) %>% 
          group_by(years) %>% 
          summarise(
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        df <- rbind(batting_player, player_summarise) %>%
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf,
            `Refuerzo` = refuerzo,
            `Logro` = resultado
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          class = 'display', # To remove lines horizontal in table
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            # autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            pageLegth = 20,
            lengthMenu = c(20, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedColumns = list(LeftColumns = 3),
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 0:26),
                              list(width = '80px', targets = 26)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      #By season -----
      # Table pitches regular season ----
      output$picheo_rs <- DT::renderDataTable({
        req(input$select_temporada)
        
        # Data ----
        player_summarise <- prs() %>% 
          filter(
            years == input$select_temporada,
            ronda == "regular"
            ) %>%
          select(-resultado, -ronda, -refuerzo) %>% 
          mutate(
            edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            whip = as.numeric(whip),
            `h/9` = as.numeric(`h/9`),
            `hr/9` = as.numeric(`hr/9`),
            `bb/9` = as.numeric(`bb/9`),
            `so/9` = as.numeric(`so/9`),
            `so/bb` = as.numeric(`so/bb`)
          ) %>%
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad, na.rm = T), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2), 
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(
            years == input$select_temporada,
            ronda == "regular"
            ) %>%
          select(-`w-l%`, -bk, -ir, -resultado, -ronda, -refuerzo, -player_id) %>%
          mutate(
            # edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            h = as.numeric(h),
            r = as.numeric(r),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            er = as.numeric(er),
            ip = as.numeric(ip),
            era = as.numeric(era),
            whip = as.numeric(round((bb + h) / ip, 2)),
            `h/9` = as.numeric(round((h/ip)*9, 2)),
            `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2)
          )
        
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            Jugador = jugador,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:23)),
                              list(width = '100px', targets = 1)
                              ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      # Table picheo round robin ----
      output$picheo_rr_sm <- DT::renderDataTable({
        req(input$select_temporada)
        
        # Data ----
        player_summarise <- prs() %>%
          filter(
            years == input$select_temporada,
            ronda == "round robin"
            ) %>%
          select(-bk, -`w-l%`, resultado, -ronda) %>%
          mutate(
            # edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            whip = as.numeric(whip),
            `h/9` = as.numeric(`h/9`),
            `hr/9` = as.numeric(`hr/9`),
            `bb/9` = as.numeric(`bb/9`),
            `so/9` = as.numeric(`so/9`),
            `so/bb` = as.numeric(`so/bb`)
          ) %>%
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            # edad = round(mean(edad), 1),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)), 
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(
            years == input$select_temporada,
            ronda == "round robin"
            ) %>%
          select(-bk, -`w-l%`, -edad, -resultado, -ronda, -ir, -player_id) %>%
          mutate(
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            # er = sum(er, na.rm = T),
            # ip = IP(ip),
            # era = as.character(round((er * 9) / ip, 2)),
            whip = as.character(round((bb + h)/ ip, 2)),
            # `h/9` = as.numeric(round((h/ip)*9, 2)),
            # `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            # `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            # `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = refuerzo
          ) 
        
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            # `Edad` = edad,
            Jugador = jugador,
            REF = refuerzo,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = c(0:23))),
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:23)),
                              list(width = '80px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      
      # Table picheo final ----
      output$picheo_finals <- DT::renderDataTable({
        req(input$select_temporada)
        
        # Data ----
        player_summarise <- prs() %>%
          filter(
            years == input$select_temporada,
            ronda == "finales"
            ) %>%
          select(-bk, -`w-l%`, -edad, -ronda) %>%
          mutate(
            w = as.numeric(w),
            l = as.numeric(l),
            era = round(as.numeric(era), 2),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            whip = as.numeric(whip),
            `h/9` = as.numeric(`h/9`),
            `hr/9` = as.numeric(`hr/9`),
            `bb/9` = as.numeric(`bb/9`),
            `so/9` = as.numeric(`so/9`),
            `so/bb` = as.numeric(`so/bb`)
          ) %>%
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = '-',
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(
            years == input$select_temporada,
            ronda == "finales"
            ) %>%
          select(-bk, -`w-l%`, -edad, -ronda, -ir, -player_id) %>%
          mutate(
            # edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            # era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            # ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            # er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            # er = sum(er, na.rm = T),
            # ip = IP(ip),
            # era = as.character(round((er * 9) / ip, 2)),
            whip = as.numeric(round((bb + h) / ip, 2)),
            # `h/9` = as.numeric(round((h/ip)*9, 2)),
            # `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            # `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            # `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            refuerzo = refuerzo,
            resultado = resultado
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            Jugador = jugador,
            REF = refuerzo,
            Resultado = resultado,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada) 
        
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        # footerCallback <- c(
        #   "function(tfoot, data, start, end, display){",
        #   "  $('th', tfoot).css('border-bottom', 'none');",
        #   "}"
        # )
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
          # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
          #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            pageLegth = 30,
            # lengthMenu = c(15, 20, 25),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:23)),
                              list(width = '100px', targets = 1)
                              ),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      
      # Table bateo regular season  ----
      output$bateo_rs <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        # Data ----
        player_summarise <- brs() %>%
          filter(
            years == input$select_temporada_bat,
            ronda == "regular"
            ) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            ir = as.numeric(ir),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) %>%
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) 
        
        
        batting_player <- brs() %>%
          filter(
            years == input$select_temporada_bat,
            ronda == "regular"
            ) %>%
          select(-ronda, -resultado, -refuerzo, -player_id) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            ir = as.numeric(ir),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) 
        
        df <- rbind(batting_player, player_summarise) %>% 
          rename(
            Temporada = years,
            Jugador = jugador,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) %>% 
          arrange(Temporada)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            pageLegth = 40,
            # lengthMenu = c(15, 20, 25),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = c(0:26))),
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:26)),
                              list(width = '100px', targets = 1)
                              ),
            # list(width = '100px', targets = 1)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      # Table bateo round robin ----
      output$bateo_rr_sm <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        # Data ----
        player_summarise <- brs() %>%
          filter(
            years == input$select_temporada_bat,
            ronda == "round robin"
                 # trimws(X5) != '' 
          ) %>%
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) %>% 
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>%
          filter(
            years == input$select_temporada_bat,
            ronda == "round robin"
                 # trimws(X5) != '' 
                 ) %>%
          select(-ronda, -resultado, -ir, -player_id) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) 
        
        df <-  rbind(player_summarise, batting_player) %>%
          rename(
            `Temporada` = years,
            `Refuerzo` = refuerzo,
            Jugador = jugador,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) %>% 
          arrange(Temporada)  
        
        # Table ----
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          class = 'display', # To remove lines horizontal in table
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:26)),
                              list(width = '100px', targets = 1)
                              ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      
      # Table bateo final ----
      output$bateo_finals <- DT::renderDataTable({
        req(input$select_temporada_bat)
        
        # Data ----
        player_summarise <- brs() %>% 
          filter(years == input$select_temporada_bat,
                 ronda == "finales",
                 trimws(pa) != '' 
          ) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) %>% 
          summarise(
            years = 'Jugadores',
            jugador = NROW(jugador),
            edad = round(mean(edad), 1),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
            resultado = last(resultado),
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>% 
          filter(
            years == input$select_temporada_bat,
            ronda == "finales",
            trimws(pa) != '' ) %>% 
          select(-ronda, -ir, -player_id) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          ) %>% 
          arrange(desc(years))
        
        
        
        df <- rbind(batting_player, player_summarise) %>%
          rename(
            `Temporada` = years,
            Jugador = jugador,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf,
            `Refuerzo` = refuerzo,
            `Logro` = resultado
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          class = 'display', # To remove lines horizontal in table
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            pageLegth = 25,
            lengthMenu = c(25, 30, 35),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            # fixedColumns = list(LeftColumns = 3),
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0, 2:26)),
                              list(width = '100px', targets = 1),
                              list(width = '85px', targets = 27)
                              ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Jugadores'), "bold")
          )
        
      })
      
      
      
      #By player ----
      # Table por Pit_rs  by jugador ----
      output$picheo_jugador <- DT::renderDataTable({
        req(input$select_jugador_pit)
        
        # Data ----
        player_summarise <- prs() %>%
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% select(key, first_name, last_name), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31, -resultado, -ronda, -refuerzo, -bk, -jugador, -player_id) %>% 
          filter(player == input$select_jugador_pit) %>%
          select(-key, -`w-l%`) %>%
          summarise(
            years = 'Total',
            edad = NROW(edad),
            edad = as.numeric(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            # ir = sum(ir, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so/bb, 2),
            # bk = sum(bk, na.rm = T),
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% select(key, name, ID, first_name, last_name) , by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>%
          select(-player, -jugador, -key, -`w-l%`, -name, -ID, -bk, -first_name, -last_name, -ir,
                 -resultado, - ronda, -refuerzo, -player_id) %>%
          arrange(edad) %>% 
          mutate(
            edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            # era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            # ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            # er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            # ir = as.numeric(ir),
            # er = sum(er, na.rm = T),
            # ip = IP(ip),
            # era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round(as.numeric(`h/9`), 2), 
            `hr/9` = round(as.numeric(`hr/9`), 2),
            `bb/9` = round(as.numeric(`bb/9`), 2),
            `so/9` = round(as.numeric(`so/9`), 2),
            `so/bb` = round(as.numeric(so / bb), 2)
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            # `IR` = ir,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada) 
        
        
        if (nrow(df) > 1) {
          df
          
        }
        else if (nrow(df) == 0) {
          " "
        }
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        # footerCallback <- c(
        #   "function(tfoot, data, start, end, display){",
        #   "  $('th', tfoot).css('border-bottom', 'none');",
        #   "}"
        # )
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
          #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            pageLegth = 15,
            lengthMenu = c(15, 20, 25),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 0:22)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      # Table por Pit_rr  by jugador ----
      output$picheo_jugador_rr <- DT::renderDataTable({
        req(input$select_jugador_pit)
        
        # Data ----
        player_summarise <- prs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>%  select(key, name, ID, first_name, last_name), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>%
          select(-jugador, -key, -`w-l%`, -name, -ID, -first_name, -last_name, -player, -bk,
                 -ronda, -resultado) %>%
          arrange(edad) %>% 
          summarise(
            years = 'Total',
            edad = NROW(edad),
            edad = as.numeric(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
            refuerzo = '-',
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>%  select(key, name, ID, first_name, last_name), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>%
          select(-jugador, -key, -`w-l%`, -name, -ID, -first_name, -last_name, -player, -bk,
                 -ronda,  -resultado, -ir, -player_id) %>%
          arrange(edad) %>%
          mutate(
            edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            # era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            # ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            # er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            # er = sum(er, na.rm = T),
            # ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round(as.numeric(`h/9`), 2), 
            `hr/9` = round(as.numeric(`hr/9`), 2),
            `bb/9` = round(as.numeric(`bb/9`), 2),
            `so/9` = round(as.numeric(`so/9`), 2),
            `so/bb` = round(so / bb, 2),
            refuerzo = refuerzo
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            Refuerzo = refuerzo,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada) 
        
        
        
        
        if (nrow(df) > 1) {
          
          # Table ----
          headerCallback <- c(
            "function(thead, data, start, end, display){",
            "  $('th', thead).css('border-bottom', 'none');",
            "}"
          )  # To deleate header line horizontal in bottom of colums name
          
          # footerCallback <- c(
          #   "function(tfoot, data, start, end, display){",
          #   "  $('th', tfoot).css('border-bottom', 'none');",
          #   "}"
          # )
          
          DT::datatable(
            df,
            extensions = "ColReorder",
            rownames = FALSE,
            style = ,
            # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
            #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
            options = list(
              dom = 'ft',  # To remove showing 1 to n of entries fields
              autoWidth = TRUE,
              searching = FALSE,
              paging = FALSE,
              pageLegth = 15,
              lengthMenu = c(15, 20, 25),
              lengthChange = FALSE,
              scrollX = TRUE,
              rownames = FALSE,
              fixedHeader = TRUE,
              fixedColumns = list(LeftColumns = 3),
              columnDefs = list(list(className = "dt-center", targets = 0:23)),
              headerCallback = JS(headerCallback),
              # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-family': 'Calibri'});",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
                "}"
              )
            )
          ) %>% 
            formatStyle(
              'Temporada',
              target = "row",
              fontWeight = styleEqual(c('Total'), "bold")
            )
          
        }
        else if (nrow(df) == 0) {
          "No tiene estadisticas"
        }
        
      })

      # Table por Pit_finals by jugador ----
      output$picheo_jugador_final <- DT::renderDataTable({
        req(input$select_jugador_pit)
        
        # Data ----
        player_summarise <- prs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>%  select(key, name, ID, first_name, last_name), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>% 
          select(-player, -key, -`w-l%`, -jugador, -name, -first_name, -last_name, -ID, 
                 -ronda) %>% 
          summarise(
            years = 'Total',
            edad = NROW(edad),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so / bb, 2),
            refuerzo = '-',
            resultado = '-',
            .groups = 'drop'
          )
        
        
        pitching_player <- prs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>%  select(key, name, ID, first_name, last_name), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>% 
          select(-player, -key, -`w-l%`, -jugador, -name, -first_name, -last_name, -ID, -bk,
                 -ronda, -ir, -player_id) %>%
          mutate(
            edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            # era = as.numeric(era),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            # ip = as.numeric(ip),
            h = as.numeric(h),
            r = as.numeric(r),
            # er = as.numeric(er),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            # er = sum(er, na.rm = T),
            # ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = round((bb + h)/ ip, 2),
            `h/9` = round(as.numeric(`h/9`), 2), 
            `hr/9` = round(as.numeric(`hr/9`), 2),
            `bb/9` = round(as.numeric(`bb/9`), 2),
            `so/9` = round(as.numeric(`so/9`), 2),
            `so/bb` = round(as.numeric(`so/bb`), 2),
            refuerzo = refuerzo,
            resultado = resultado
          ) 
        
        df <- rbind(pitching_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            Refuerzo = refuerzo,
            Resultado = resultado,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`) %>% 
          arrange(Temporada) 
        
        
        if (nrow(df) > 1) {
          
          # Table ----
          headerCallback <- c(
            "function(thead, data, start, end, display){",
            "  $('th', thead).css('border-bottom', 'none');",
            "}"
          )  # To deleate header line horizontal in bottom of colums name
          
          # footerCallback <- c(
          #   "function(tfoot, data, start, end, display){",
          #   "  $('th', tfoot).css('border-bottom', 'none');",
          #   "}"
          # )
          
          DT::datatable(
            df,
            extensions = "ColReorder",
            rownames = FALSE,
            style = ,
            # callback = JS(c("$('table.dataTable thead th').css('border-bottom', 'none');",
            #                 "$('table.dataTable.no-footer').css('border-top', 'none');")),
            options = list(
              dom = 'ft',  # To remove showing 1 to n of entries fields
              autoWidth = TRUE,
              searching = FALSE,
              paging = FALSE,
              pageLegth = 15,
              lengthMenu = c(15, 20, 25),
              lengthChange = FALSE,
              scrollX = TRUE,
              rownames = FALSE,
              fixedHeader = TRUE,
              fixedColumns = list(LeftColumns = 3),
              columnDefs = list(list(className = "dt-center", targets = 0:24)),
              headerCallback = JS(headerCallback),
              # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-family': 'Calibri'});",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
                "}"
              )
            )
          ) %>% 
            formatStyle(
              'Temporada',
              target = "row",
              fontWeight = styleEqual(c('Total'), "bold")
            )
          
        }
        else if (nrow(df) == 0) {
          "No tiene estadisticas"
        }
        
        
      })
      
      # Table por Bat_rs  by jugador ----
      output$bat_rs <- DT::renderDataTable({
        req(input$select_jugador_bat)
        
        # Data ----
        player_summarise <- brs() %>%
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31) %>% 
          filter(player == input$select_jugador_bat) %>%
          select(-player) %>%
          summarise(
            years = 'Temporadas',
            edad = NROW(edad),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31) %>% 
          filter(player == input$select_jugador_bat) %>% 
          select(-jugador, -player, -key, -ir, -player_id, -ronda, -resultado, -refuerzo) %>% 
          mutate(
            g = as.numeric(g),
            # X5 = as.numeric(X5),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3)
          )
        
        df <- rbind(batting_player, player_summarise) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) %>% 
          arrange(Temporada) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            pageLegth = 15,
            lengthMenu = c(15, 20, 25),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 0:24),
                              list(width = '100px', targets = 1)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Temporadas'), "bold")
          )
        
      })
      
      
      # Table por Bat_rr  by jugador ----
      output$bat_rr <- DT::renderDataTable({
        req(input$select_jugador_bat)
        
        player_summarise <- brs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31) %>% 
          filter(player == input$select_jugador_bat) %>% 
          select(-jugador) %>% 
          summarise(
            years = 'Temporadas',
            edad = NROW(edad),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = '',
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31) %>% 
          filter(player == input$select_jugador_bat) %>% 
          select(-jugador, -player, -key, -player_id, -ronda, -resultado, -ir) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          )
        
        df <-  rbind(player_summarise, batting_player) %>%
          rename(
            `Temporada` = years,
            `Refuerzo` = refuerzo,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) %>% 
          arrange(Temporada)  
        
        
        if (nrow(df) > 1) {
          # Datatable ----
          headerCallback <- c(
            "function(thead, data, start, end, display){",
            "  $('th', thead).css('border-bottom', 'none');",
            "}"
          )  # To deleate header line horizontal in bottom of colums name
          
          DT::datatable(
            df,
            class = 'display', # To remove lines horizontal in table
            extensions = "ColReorder",
            rownames = FALSE,
            options = list(
              # autoWidth = TRUE,
              dom = 'ft',  # To remove showing 1 to n of entries fields
              searching = FALSE,
              paging = FALSE,
              pageLegth = 15,
              lengthMenu = c(15, 20, 25),
              lengthChange = FALSE,
              scrollX = TRUE,
              rownames = FALSE,
              fixedHeader = TRUE,
              fixedColumns = list(LeftColumns = 3),
              columnDefs = list(list(className = "dt-center", targets = 0:25),
                                list(width = '100px', targets = 1)),
              headerCallback = JS(headerCallback),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-family': 'Calibri'});",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
                "}"
              )
            )
          ) %>% 
            formatStyle(
              'Temporada',
              target = "row",
              fontWeight = styleEqual(c('Temporadas'), "bold")
            )
 
          
        }
        else if (nrow(df) == 0) {
          "No tiene estadisticas"
        }
        
        
        
      })
      
      # Table por Bat_finals  by jugador ----
      output$bat_final <- DT::renderDataTable({
        req(input$select_jugador_bat)
        
        
        player_summarise <- brs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:32) %>% 
          filter(player == input$select_jugador_bat) %>% 
          select(-jugador) %>% 
          summarise(
            years = 'Temporadas',
            edad = NROW(edad),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            refuerzo = '',
            .groups = 'drop'
          )
        
        
        batting_player <- brs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          left_join(Rosters() %>% 
                      select(first_name, last_name, key), by = 'key') %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          select(key, player, 1:31) %>% 
          filter(player == input$select_jugador_bat) %>% 
          select(-jugador, -player, -key, -player_id, -ronda, -resultado, -ir) %>% 
          mutate(
            edad = as.numeric(edad),
            g = as.numeric(g),
            pa = as.numeric(pa),
            ab = as.numeric(ab),
            r = as.numeric(r),
            h = as.numeric(h),
            hr = as.numeric(hr),
            rbi = as.numeric(rbi),
            sb = as.numeric(sb),
            cs = as.numeric(cs),
            bb = as.numeric(bb),
            so = as.numeric(so),
            avg = round(h/ab, 3),
            obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = as.numeric(rc),
            tb = as.numeric(tb),
            xb = as.numeric(xb),
            hbp = as.numeric(hbp),
            sh = as.numeric(sh),
            sf = as.numeric(sf),
            `2b` = as.numeric(`2b`),
            `3b` = as.numeric(`3b`)
          )

        
        
        df <- rbind(batting_player, player_summarise) %>%
          select(years, 2:26, refuerzo) %>% 
          rename(
            `Temporada` = years,
            `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf,
            `Refuerzo` = refuerzo
          ) %>% 
          arrange(Temporada) 
        
        
        if (nrow(df) > 1) {
          # Datatable ----
          headerCallback <- c(
            "function(thead, data, start, end, display){",
            "  $('th', thead).css('border-bottom', 'none');",
            "}"
          ) # To delete header line horizontal in bottom of columns name
          
          DT::datatable(
            df,
            class = 'display', # To remove lines horizontal in table
            extensions = "ColReorder",
            rownames = FALSE,
            options = list(
              # autoWidth = TRUE,
              dom = 'ft',  # To remove showing 1 to n of entries fields
              searching = FALSE,
              paging = FALSE,
              pageLegth = 15,
              lengthMenu = c(15, 20, 25),
              lengthChange = FALSE,
              scrollX = TRUE,
              rownames = FALSE,
              fixedColumns = list(LeftColumns = 3),
              fixedHeader = TRUE,
              columnDefs = list(list(className = "dt-center", targets = 0:25),
                                list(width = '50px', targets = 25)
              ),
              headerCallback = JS(headerCallback),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-family': 'Calibri'});",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
                "}"
              )
            )
          ) %>% 
            formatStyle(
              'Temporada',
              target = "row",
              fontWeight = styleEqual(c('Temporadas'), "bold")
            )
          
          
        }
        else if (nrow(df) == 0) {
          "No tiene estadisticas"
        }
        
        
      })
      
      #By position ----
      # Table bateo regular season by P ----
      output$info_position_rs_pit <- DT::renderDataTable({
        req(input$select_posicion_pit)

        # Data ----
        df <- prs() %>%
          filter(ronda == "regular") %>% 
          mutate(key = paste(as.character(years), jugador)) %>%
          select(key, 1:28) %>%
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, pos, first_name, last_name, ID), by = "key"
          ) %>% 
          select(-key, -`w-l%`, -years) %>%
          group_by(ID) %>%
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so/bb, 2),
            .groups = 'drop'
          ) %>%
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 4:24) %>%
          arrange(desc(ip)) %>%
          rename(
            Jugador = player,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
            )
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:20))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

        

      })
      
      
      # Table bateo round robin by P ----
      output$info_position_rr_pit <- DT::renderDataTable({
        req(input$select_posicion_pit)

        # Data ----
        df <- prs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>% 
          # mutate(key = paste(as.character(years), jugador)) %>%
          # select(key, 1:28) %>%
          # left_join(Rosters %>%
          #             mutate(key = paste(as.character(years), jugador)) %>%
          #             select(key, pos, first_name, last_name, ID), by = "key"
          # ) %>% 
          select(-`w-l%`, -years) %>%
          group_by(player_id) %>%
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so/bb, 2),
            .groups = 'drop'
          ) %>%
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 4:24) %>%
          arrange(desc(ip)) %>%
          rename(
            Jugador = player,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
            )
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:20))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

        

      })
      
      
      # Table bateo final by P ----
      output$info_position_final_pit <- DT::renderDataTable({
        req(input$select_posicion_pit)

        # Data ----
        df <- prs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>% 
          group_by(player_id) %>%
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = round((h/ip)*9, 2),
            `hr/9` = round((hr/ip)*9, 2),
            `bb/9` = round((bb/ip)*9, 2),
            `so/9` = round((so/ip)*9, 2),
            `so/bb` = round(so/bb, 2),
            .groups = 'drop'
          ) %>%
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 4:24) %>%
          arrange(desc(ip)) %>%
          rename(
            Jugador = player,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
            )
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        DT::datatable(
          df,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:20))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

        

      })
      
      
      # Table bateo regular season by position players ----
      output$info_position_bat <- DT::renderDataTable({
        req(input$select_posicion_bat)
        
        # Data ----
        batting_player <- brs() %>%
          mutate(key = paste(as.character(years), jugador)) %>%
          select(key, 1:28) %>%
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, pos, first_name, last_name), by = "key"
          ) %>%
          select(2:3, pos, first_name, last_name, 3:31) %>%
          arrange(years, jugador)  %>%
          select(-years, -edad) %>%
          filter(pos == input$select_posicion_bat) %>% 
          group_by(jugador)  %>%
          summarise(
            pos  = last(pos),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 5:27) %>%
          arrange(desc(g)) %>%
          rename(
            Jugador = player,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          )
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        # Table ----
        DT::datatable(
          batting_player,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:23))),
            # columnDefs = list(
            #   list(
            #     className = "dt-center", targets = 0:23)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

      })
      
      
      # Table bateo round robin by position players ----
      output$info_position_rr_bat <- DT::renderDataTable({
        req(input$select_posicion_bat)
        
        # Data ----
        batting_player <- brs() %>%
          filter(ronda == "round robin") %>% 
          mutate(key = paste(as.character(years), jugador)) %>%
          select(key, 1:28) %>%
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, pos, first_name, last_name), by = "key"
          ) %>%
          select(2:3, pos, first_name, last_name, 3:31) %>%
          arrange(years, jugador)  %>%
          select(-years, -edad) %>%
          filter(pos == input$select_posicion_bat) %>% 
          group_by(jugador)  %>%
          summarise(
            pos  = last(pos),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 5:27) %>%
          arrange(desc(g)) %>%
          rename(
            Jugador = player,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          )
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        # Table ----
        DT::datatable(
          batting_player,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:23))),
            # columnDefs = list(
            #   list(
            #     className = "dt-center", targets = 0:23)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

      })
      
      
      # Table bateo final by position players ----
      output$info_position_final_bat <- DT::renderDataTable({
        req(input$select_posicion_bat)
        
        # Data ----
        batting_player <- brs() %>%
          filter(ronda == "finales") %>% 
          mutate(key = paste(as.character(years), jugador)) %>%
          select(key, 1:28) %>%
          left_join(Rosters() %>%
                      mutate(key = paste(as.character(years), jugador)) %>%
                      select(key, pos, first_name, last_name), by = "key"
          ) %>%
          select(2:3, pos, first_name, last_name, 3:31) %>%
          arrange(years, jugador)  %>%
          select(-years, -edad) %>%
          filter(pos == input$select_posicion_bat) %>% 
          group_by(jugador)  %>%
          summarise(
            pos  = last(pos),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round((h - `2b` - `3b` - hr + (2 *`2b`) + (3 * `3b`)+  (4 * hr))/ ab, 3),
            ops = round(slg + obp, 3),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          mutate(player = paste0(first_name, " ", last_name, sep = " ")) %>%
          select(player, 5:27) %>%
          arrange(desc(g)) %>%
          rename(
            Jugador = player,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          )
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        ) 
        
        
        # Table ----
        DT::datatable(
          batting_player,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            htmltools::em('Las estadísticas PA, OBP, OPS, CS, SB, BB, SO, HBP, RC, SH y SF
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(
              list(
                width = '120px', targets = 0,
                width = '10px', targets = c(1:23))),
            # columnDefs = list(
            #   list(
            #     className = "dt-center", targets = 0:23)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )

      })
      
      
      #By country ----
      # Table pitching regular season by country ----
      output$picheo_rs_country <- renderDataTable({
        req(input$select_country)
        
        # Data ----
        pit_geographic <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>%
          group_by(years) %>% 
          summarise(
            jugador = n(),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years)) 

          
        
        
        
        country_summarise <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>%
          summarise(
            years = "Total",
            jugador = n(),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) 
          
        
        df <- rbind(pit_geographic, country_summarise) %>% 
          rename(
            # `Pais` = pais,
            `Temporada` = years,
            # `Edad` = edad,
            `Jugadores` = jugador,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            # `GP` = gp,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            # IR = ir,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            # `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
                              # list(width = '60px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      #By Rosters 
      # Table pitching round robin by country ----
      output$picheo_rr_country <- renderDataTable({
        req(input$select_country)
        
        # Data ----
        pit_geographic <- prs() %>% 
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>% 
          group_by(years) %>% 
          summarise(
            jugador = n_distinct(jugador),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        
        
        
        country_summarise <- prs() %>% 
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>% 
          summarise(
            years = "Total",
            jugador = n_distinct(jugador),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
            ) 
         
        
        
        df <- rbind(pit_geographic, country_summarise) %>% 
          rename(
            # `Pais` = pais,
            `Temporada` = years,
            # `Edad` = edad,
            `Jugadores` = jugador,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            # `GP` = gp,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            # IR = ir,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            # `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
                              # list(width = '120px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      #By Rosters 
      # Table pitching finales by country ----
      output$picheo_finals_country <- renderDataTable({
        req(input$select_country)
        
        # Data ----
        pit_geographic <- prs() %>% 
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>% 
          group_by(years) %>% 
          summarise(
            jugador = n_distinct(jugador),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        
        
        country_summarise <- prs() %>% 
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          filter(pais == input$select_country) %>% 
          summarise(
            years = "Total",
            jugador = n_distinct(jugador),
            w = as.character(sum(w, na.rm = T)),
            l = as.character(sum(l, na.rm = T)),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            .groups = 'drop'
          ) 
        
        
        
        df <- rbind(pit_geographic, country_summarise) %>% 
          rename(
            # `Pais` = pais,
            `Temporada` = years,
            # `Edad` = edad,
            `Jugadores` = jugador,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            # `GP` = gp,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            # IR = ir,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            # `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          )
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
                              # list(width = '120px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
      })
      
      #By Rosters 
      # Table Geo Pitching stats by country ----
      output$country_pit <- renderDataTable({
        
        # Data ----
        pais_pit <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          group_by(pais) %>% 
          summarise(
            jugador = n_distinct(jugador),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            # ir = sum(ir, na.rm = T),
            whip = as.character(round(sum(bb, h, na.rm = TRUE)/ ip, 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(so/bb, 2)),
            .groups = "drop"
          ) %>% 
          arrange(desc(w)) %>% 
          rename(
            `Pais` = pais,
            # `Temporada` = years,
            # `Edad` = edad,
            `Jugadores` = jugador,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            # `GP` = gp,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            # IR = ir,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) %>% 
          filter(!is.na(Pais))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          pais_pit,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores a los cuales se desconoce su país. Se asigna "Desconocido"')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(1:21)),
                              list(width = '130px', targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
        
        
      })
      # Table batting regular season by country ----
      output$bateo_rs_country <- renderDataTable({
        req(input$select_country_bat)
        
        # Data ----
        country_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>% 
          group_by(years) %>% 
          summarise(
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years)) 
          
        
        
        country_summarise <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>%
          summarise(
            years = 'Total',
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          )
        
        
        
        
        df <- rbind(country_bat, country_summarise) %>% 
          rename(
            # Pais = pais,
            Jugadores = jugador,
            Temporada = years,
            # `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas CS, BB, SO, OBP, SLG, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:24))),
                              # list(width = '140px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      # Table batting round robin by country ----
      output$bateo_rr_country <- renderDataTable({
        req(input$select_country_bat)
        
        # Data ----
        country_bat <- brs() %>% 
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>% 
          group_by(years) %>% 
          summarise(
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        
        
        country_summarise <- brs() %>% 
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>% 
          summarise(
            years = 'Total',
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) 
          
        
        
        
        df <- rbind(country_bat, country_summarise) %>% 
          rename(
            # Pais = pais,
            Jugadores = jugador,
            Temporada = years,
            # `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas CS, BB, SO, OBP, SLG, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:24))),
                              # list(width = '140px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      # Table batting finales season by country ----
      output$bateo_finals_country <- renderDataTable({
        req(input$select_country_bat)
        
        # Data ----
        country_bat <- brs() %>% 
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>% 
          group_by(years) %>% 
          summarise(
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years))
        
        
        
        country_summarise <- brs() %>% 
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          filter(pais == input$select_country_bat) %>% 
          summarise(
            years = 'Total',
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) 
        
        
        
        
        df <- rbind(country_bat, country_summarise) %>% 
          rename(
            # Pais = pais,
            Jugadores = jugador,
            Temporada = years,
            # `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          )  
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          df,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadisticas CS, BB, SO, OBP, SLG, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            ordering = F, # To delete Ordering
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:24))),
                              # list(width = '140px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        
      })
      
      # Table Geo Batting stats by country ----
      output$country_bat <- renderDataTable({
        
        # Data ----
        country_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          group_by(pais) %>% 
          summarise(
            jugador = n_distinct(jugador),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(ab)) %>% 
          rename(
            Pais = pais,
            Jugadores = jugador,
            # Temporada = years,
            # `Edad` = edad,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) %>% 
          filter(!is.na(Pais))
        
        # Table ----
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          country_bat,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores a los cuales se desconoce su país. Se asigna "Desconocido"')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(1:24)),
                              list(width = '135px', targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
        
        
        
      })
      #By Roster ----
      # Table by roster ----
      output$info_roster <- DT::renderDataTable({
        
        # Data ----
        roster <- Rosters() %>%
          filter(ronda == "regular") %>% 
          # filter(years == input$select_rosters) %>% 
          mutate(
            jugador = paste0(first_name, " ", last_name),
            flag = case_when(
              pais == "Venezuela" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>',
              pais == "Republica Dominicana" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/9/9f/Flag_of_the_Dominican_Republic.svg" width="25" height="15"></img>',
              pais == "Puerto Rico" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/2/28/Flag_of_Puerto_Rico.svg" width="25" height="15"></img>',
              pais == "Cuba" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/b/bd/Flag_of_Cuba.svg" width="25" height="15"></img>',
              pais == "Mexico" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/f/fc/Flag_of_Mexico.svg" width="25" height="15"></img>',
              pais == "Japon" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/9/9e/Flag_of_Japan.svg" width="25" height="15"></img>',
              pais == "USA" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>',
              pais == "Francia" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/c/c3/Flag_of_France.svg" width="25" height="15"></img>',
              pais == "Colombia" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/2/21/Flag_of_Colombia.svg" width="25" height="15"></img>',
              pais == "Brasil" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/0/05/Flag_of_Brazil.svg" width="25" height="15"></img>',
              pais == "Panama" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/a/ab/Flag_of_Panama.svg" width="25" height="15"></img>',
              pais == "Alemania" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/b/ba/Flag_of_Germany.svg" width="25" height="15"></img>',
              pais == "Nueva Zelanda" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/3/3e/Flag_of_New_Zealand.svg" width="25" height="15"></img>',
              pais == "Reino Unido" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/a/ae/Flag_of_the_United_Kingdom.svg" width="25" height="15"></img>',
              pais == "Canada" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/d/d9/Flag_of_Canada_%28Pantone%29.svg" width="25" height="15"></img>',
              pais == "Australia" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/8/88/Flag_of_Australia_%28converted%29.svg" width="25" height="15"></img>',
              pais == "Espana" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/8/89/Bandera_de_Espa%C3%B1a.svg" width="25" height="15"></img>',
              pais == "Rusia" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/f/f3/Flag_of_Russia.svg" width="25" height="15"></img>',
              pais == "Curazao" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/b/b1/Flag_of_Cura%C3%A7ao.svg" width="25" height="15"></img>',
              pais == "Honduras" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/8/82/Flag_of_Honduras.svg" width="25" height="15"></img>',
              pais == "Aruba" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/f/f6/Flag_of_Aruba.svg" width="25" height="15"></img>',
              pais == "Bahamas" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/9/93/Flag_of_the_Bahamas.svg" width="25" height="15"></img>',
              pais == "Nicaragua" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/1/19/Flag_of_Nicaragua.svg" width="25" height="15"></img>',
              pais == "El Salvador" ~ '<img src="https://upload.wikimedia.org/wikipedia/commons/3/34/Flag_of_El_Salvador.svg" width="25" height="15"></img>'
              )
            ) %>% 
          select(years, flag, jugador, pos, bat, lan, pais, estado, ciudad) 
          
        
        # # Filter data based on selections
        if (input$select_rosters != "Todas las Temporadas") {
          roster  <- filter(roster , years == input$select_rosters)
        }
        if (input$countrys != "Todos los Paises") {
          roster <- filter(roster, pais == input$countrys)
        }
        if (input$posiciones != "Todas las Posiciones") {
          roster <- filter(roster, pos == input$posiciones)
        }
        
        
        roster <- roster %>% 
          rename(
            Temporada = years,
            Jugador = jugador,
            `Posicion` = pos,
            `Batea` = bat,
            `Lanza` = lan,
            Pais = pais,
            `Estado` = estado,
            `Ciudad` = ciudad,
            " " = flag
            ) %>% 
          arrange(Jugador)
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          escape = FALSE,
          roster,
          extensions = "ColReorder",
          rownames = FALSE,
          style = ,
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = FALSE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            columnDefs = list(list(className = "dt-center", targets = c(0:5)),
                              list(width = '130px', targets = c(2, 6:8))
                              ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )  
        
      })
      
      
      #Pitching Record ----
      # Table picheo lideres w ----
      output$p_w <- renderDataTable({
        
        # Data ----
        w <- prs()%>%
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          # left_join(Rosters %>%
          #             mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          #             select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>%
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            w = sum(w, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>%
          ungroup() %>%
          arrange(desc(w)) %>%
          select(img, first_name, last_name, w) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>%
          top_n(10, w) %>%
          rename(
            Jugador = jugador,
            W = w,
            " " = img
          ) %>%
          slice(1:(n()-1))

        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          w,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 1),
            #                   list(width = '120px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres l ----
      output$p_l <- renderDataTable({
        
        # Data ----
        l <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            l = sum(l, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(l)) %>%
          select(img, first_name, last_name, l) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, l) %>% 
          rename(
            " " = img,
            Jugador = jugador,
            L = l
          ) %>% 
          slice(1:(n()-1)) 
          
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          l,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres g ----
      output$p_g <- renderDataTable({
        
        # Date ----
        g <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            g = sum(g, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(g)) %>%
          select(img, first_name, last_name, g) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, g) %>% 
          rename(
            Jugador = jugador,
            G = g, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          g,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres gs ----
      output$p_gs <- renderDataTable({
        
        # Data ----
        gs <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            gs = sum(gs, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(gs)) %>%
          select(img, first_name, last_name, gs) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, gs) %>% 
          rename(
            Jugador = jugador,
            GS = gs, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          gs,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres ip ----
      output$p_ip <- renderDataTable({
        
        # Data ----
        ip <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(ip)) %>%
          select(img, first_name, last_name, ip) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, ip) %>% 
          rename(
            Jugador = jugador,
            IP = ip,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so ----
      output$p_so <- renderDataTable({
        
        # Data ----
        so <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            so = sum(so, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(so)) %>%
          select(img, first_name, last_name, so) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, so) %>% 
          rename(
            Jugador = jugador,
            SO = so,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres h ----
      output$p_h <- renderDataTable({
        
        # Data ----
        h <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            h = sum(h, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(h)) %>%
          select(img, first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, h) %>% 
          rename(
            Jugador = jugador,
            H = h,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table picheo lideres bb ----
      output$p_bb <- renderDataTable({
        
        # Data ----
        bb <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            bb = sum(bb, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(bb)) %>%
          select(img, first_name, last_name, bb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, bb) %>% 
          rename(
            Jugador = jugador,
            BB = bb,
            " " = img
          ) 
        # %>% 
        #   slice(1:(n()-3))
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table picheo lideres era ----
      output$p_era <- renderDataTable({
        
        # Data ----
        era <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(era) %>%
          select(img, first_name, last_name, era) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_frac(10, era) %>% 
          rename(
            Jugador = jugador,
            ERA = era, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          era,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres sv ----
      output$p_sv <- renderDataTable({
        
        # Data ----
        sv <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            sv = sum(sv, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            bk = sum(bk, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(sv)) %>%
          select(img, first_name, last_name, sv) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, sv) %>% 
          rename(
            Jugador = jugador,
            SV = sv,
            " " = img
          ) %>% 
          slice(1:(n()-1))
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sv,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres whip ----
      output$p_whip <- renderDataTable({
        
        # Data ----
        whip <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            bb = sum(bb, na.rm = T),
            whip = as.character(round((bb + h)/ ip, 2)),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(whip) %>%
          select(img, first_name, last_name, whip) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, whip) %>%
          rename(
            Jugador = jugador,
            WHIP = whip, 
            " " = img
            ) 
        # 
        # %>% 
        #   slice(1:n()-1)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          whip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres h/9 ----
      output$p_h9 <- renderDataTable({
        
        # Data ----
        h_9 <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            h = sum(h, na.rm = T),
            `h/9` = as.character(round((h/ip)*9, 2)),
            bk = sum(bk, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(`h/9`) %>%
          select(img, first_name, last_name, `h/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `h/9`) %>%
          rename(
            Jugador = jugador,
            `H/9` = `h/9`,
            " " = img
          ) 
        
        # %>% 
        #   slice(1:(n()-1))
        # 
        # Table ---- 
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so/9 ----
      output$p_so9 <- renderDataTable({
        
        # Data ----
        so_9 <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            so = sum(so, na.rm = T),
            `so/9` = as.character(round((so/ip)*9, 2)),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            bk = sum(bk, na.rm = T),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(desc(`so/9`)) %>%
          select(img, first_name, last_name, `so/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `so/9`) %>%
          rename(
            Jugador = jugador,
            `SO/9` = `so/9`, 
            " " = img
          ) 
        
        # %>% 
        #   slice(1:(n()-1))
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres bb/9 ----
      output$p_bb9 <- renderDataTable({
        
        # Data ----
        bb_9 <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            bb = sum(bb, na.rm = T),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(`bb/9`) %>%
          select(img, first_name, last_name, `bb/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `bb/9`) %>%
          rename(
            Jugador = jugador,
            `BB/9` = `bb/9`,
             " " = img
          ) 
        
        # %>% 
        #   slice(1:(n()-1)) %>% 
        #   ungroup()
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so/bb ----
      output$p_sobb <- renderDataTable({
        
        # Data ----
        so_bb <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(player_id, 1:28) %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, name, ID, first_name, last_name), 
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          group_by(player_id) %>% 
          summarise(
            first_name = last(first_name),
            last_name = last(last_name),
            jugador= last(jugador),
            ip = IP(ip),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            `so/bb` = as.character(round(so /bb, 2)),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ip > 400) %>% 
          arrange(desc(`so/bb`)) %>%
          select(img, first_name, last_name, `so/bb`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `so/bb`) %>%
          rename(
            Jugador = jugador,
            `SO/BB` = `so/bb`, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 400 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 0),
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
        
      })
      
      #Batting Records -----
      # Table bateo lideres H ----
      output$b_hits <- renderDataTable({
        
        # Data ----
        hits <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(h)) %>%
          select(img, first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, h) %>% 
          rename(
            Jugador = jugador,
            H = h,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hits,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 1)),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres 2B ----
      output$b_2b <- renderDataTable({
        
        # Data ----
        dobles <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(`2b`)) %>%
          select(img, first_name, last_name, `2b`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `2b`) %>% 
          rename(
            Jugador = jugador,
            `2B` = `2b`, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          dobles,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
                              # list(width = '120px', targets = 1)
                              # ),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres 3B ----
      output$b_3b <- renderDataTable({
        
        # Data ----
        triples <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(`3b`)) %>%
          select(img, first_name, last_name, `3b`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, `3b`) %>% 
          rename(
            Jugador = jugador,
            `3B` = `3b`,
            " " = img
          ) %>% 
          slice(1:(n()-2))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          triples,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres HR ----
      output$b_hr <- renderDataTable({
        
        # Data ----
        hr <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(hr)) %>%
          select(img, first_name, last_name, hr) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, hr) %>% 
          rename(
            Jugador = jugador,
            HR = hr,
            " " = img
          ) 
        
        # %>% 
        #   slice(1:(n()-3))
        # 
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hr,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres AVG ----
      output$b_average <- renderDataTable({
        
        # Data ----
        avg <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          filter(ab >= 1500) %>% 
          arrange(desc(avg)) %>% 
          select(img, first_name, last_name, h, ab, avg) %>% 
          mutate(avg = round(((h)/ ab), 3)) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, avg) %>% 
          arrange(desc(avg)) %>% 
          mutate(Order = seq(1, NROW(jugador), 1)) %>% 
          select(img, jugador, avg) %>% 
          rename(
            Jugador = jugador,
            AVG = avg,
            " " = img
            ) 
          
        
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          avg,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 1550 AB')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres OBP ----
      output$b_obp <- renderDataTable({
        
        # Data ----
        obp <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(years, ID, key, first_name,last_name, jugador, 2:29) %>%
          filter(years %in% temporadas_batting) %>% 
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(obp)) %>% 
          filter(ab >= 500) %>% 
          select(img, first_name, last_name, obp) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, obp) %>% 
          select(img, jugador, obp) %>% 
          rename(
            Jugador = jugador,
            OBP = obp,
            " " = img
          ) %>% 
          arrange(desc(OBP)) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          obp,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Desde 2005-06')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres SLG ----
      output$b_slg <- renderDataTable({
        
        # Data ----
        slg <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(years, ID, key, first_name,last_name, jugador, 2:29) %>%
          filter(years %in% temporadas_batting) %>% 
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(slg)) %>% 
          filter(ab >= 500) %>% 
          select(img, first_name, last_name, slg) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, slg) %>% 
          select(img, jugador, slg) %>% 
          rename(
            Jugador = jugador,
            SLG = slg,
            " " = img
          ) %>% 
          arrange(desc(SLG)) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          slg,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Desde 2005-06')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres OPS ----
      output$b_ops <- renderDataTable({
        
        # Data ----
        ops <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(years, ID, key, first_name,last_name, jugador, 2:29) %>%
          filter(years %in% temporadas_batting) %>% 
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(ops)) %>% 
          filter(ab >= 500) %>% 
          select(img, first_name, last_name, ops) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, ops) %>% 
          select(img, jugador, ops) %>% 
          rename(
            Jugador = jugador,
            OPS = ops,
            " " = img
          ) %>% 
          arrange(desc(OPS)) 
        
        
        # Table ---- 
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ops,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Desde 2005-06')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres RBI ----
      output$b_rbi <- renderDataTable({
        
        # Data ----
        rbi <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(rbi)) %>%
          select(img, first_name, last_name, rbi) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, rbi) %>% 
          rename(
            Jugador = jugador,
            RBI = rbi, 
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          rbi,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres AB ----
      output$b_ab <- renderDataTable({
        
        # Data ----
        ab <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(ab)) %>%
          select(img, first_name, last_name, ab) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, ab) %>% 
          rename(
            Jugador = jugador,
            AB = ab, 
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ab,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres SB ----
      output$b_sb <- renderDataTable({
        
        # Data ----
        sb <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(sb)) %>%
          select(img, first_name, last_name, sb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, sb) %>% 
          rename(
            Jugador = jugador,
            SB = sb, 
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres XB ----
      output$b_xb <- renderDataTable({
        
        # Data ----
        xb <-brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          group_by(ID) %>% 
          summarise(
            years = NROW(years),
            first_name = last(first_name),
            last_name = last(last_name),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(mean(avg, na.rm = T), 3),
            obp = round(mean(obp, na.rm = T), 3),
            slg = round(mean(slg, na.rm = T), 3),
            ops = round(mean(ops, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>'),
            .groups = 'drop'
          ) %>% 
          arrange(desc(xb)) %>%
          select(img, first_name, last_name, xb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, xb) %>% 
          rename(
            Jugador = jugador,
            XB = xb, 
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          xb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:2))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      #Pitching Record by season 
      # Pitching Record by season ----
      # Table picheo lideres w ----
      output$pt_p_w <- renderDataTable({
        
        # Data ----
        w <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(w)) %>%
          select(years, first_name, last_name, w) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, w) %>% 
          select(img, jugador, years, w) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            W = w,
            " " = img
          ) %>% 
          slice(1:(n()-1))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          w,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres l ----
      output$pt_p_l <- renderDataTable({
        
        # Data ----
        l <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>% 
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(l)) %>%
          select(years,first_name, last_name, l) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, l) %>% 
          select(img, jugador, years, l) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            L = l,
            " " = img
          ) %>% 
          slice(1:(n()-11)) 
          
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          l,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres g ----
      output$pt_p_g <- renderDataTable({
        
        # Data ----
        g <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(g)) %>%
          select(years,first_name, last_name, g) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, g) %>% 
          select(img, jugador, years, g) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            G = g,
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          g,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres gs ----
      output$pt_p_gs <- renderDataTable({
        
        # Data ----
        gs <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(gs)) %>%
          select(years, first_name, last_name, gs) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, gs) %>% 
          select(img, jugador, years, gs) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            GS = gs,
            " " = img
          ) %>% 
          slice(1:(n()-9)) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          gs,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres ip ----
      output$pt_p_ip <- renderDataTable({
        
        # Data ----
        ip <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(ip)) %>%
          select(years, first_name, last_name, ip) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, ip) %>% 
          select(img, jugador, years, ip) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            IP = ip,
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so ----
      output$pt_p_so <- renderDataTable({
        
        # Data ----
        so <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(so)) %>%
          select(years, first_name, last_name, so) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, so) %>% 
          select(img, jugador, years, so) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            SO = so,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres h ----
      output$pt_p_h <- renderDataTable({
        
        # Data ----
        h <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(h)) %>%
          select(years, first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, h) %>% 
          select(img, jugador, years, h) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            H = h,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table picheo lideres bb ----
      output$pt_p_bb <- renderDataTable({
        
        # Data ----
        bb <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          arrange(desc(bb)) %>%
          select(years, first_name, last_name, bb) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_n(10, bb) %>% 
          select(img, jugador, years, bb) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            BB = bb,
            " " = img
          ) %>% 
          slice(1:(n()-3)) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 1)),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table picheo lideres era ----
      output$pt_p_era <- renderDataTable({
        
        # Data ----
        era <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          filter(ip > 50) %>% 
          arrange(era) %>%
          select(years, first_name, last_name, era) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          top_frac(10, era) %>% 
          select(img, jugador, years, era) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            ERA = era,
            " " = img
          ) %>% 
          slice(1:10) 
          
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          era,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 1)),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres whip ----
      output$pt_p_whip <- renderDataTable({
        
        # Data ----
        whip <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          mutate(
            w = as.numeric(w),
            l = as.numeric(l),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            h = as.numeric(h),
            r = as.numeric(r),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            er = as.numeric(er),
            ip = as.numeric(ip),
            era = as.numeric(era),
            whip = as.numeric(round((bb + h) / ip, 2)),
            `h/9` = as.numeric(round((h/ip)*9, 2)),
            `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')
          ) %>% 
          filter(ip > 50) %>% 
          arrange(whip) %>%
          select(years, first_name, last_name, whip, img) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(-10, whip) %>%
          select(img, jugador, years, whip) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            WHIP = whip,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          whip,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = 1)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres sv ----
      output$pt_p_sv <- renderDataTable({
        
        # Data ----
        sv <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(sv)) %>%
          select(years, first_name, last_name, sv, img) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, sv) %>% 
          select(img, jugador, years, sv) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            SV = sv,
            " " = img
            ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sv,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres h/9 ----
      output$pt_p_h9 <- renderDataTable({
        
        # Data ----
        h_9 <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          filter(ip > 50) %>% 
          arrange(`h/9`) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          select(years, first_name, last_name, `h/9`, img) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, `h/9`) %>%
          top_n(-10, `h/9`) %>%
          rename(
            Año = years,
            Jugador = jugador,
            `H/9` = `h/9`,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          h_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so/9 ----
      output$pt_p_so9 <- renderDataTable({
        
        # Table ----
        so_9 <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          filter(ip > 50) %>% 
          arrange(desc(`so/9`)) %>%
          select(years, first_name, last_name, `so/9`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          select(img, jugador, years, `so/9`) %>%
          top_n(10, `so/9`) %>%
          rename(
            Año = years,
            Jugador = jugador,
            `SO/9` = `so/9`,
            " " = img
          ) 
        
        
        # Data ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres bb/9 ----
      output$pt_p_bb9 <- renderDataTable({
        
        # Data ----
        bb_9 <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>% 
          mutate(
            # edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            h = as.numeric(h),
            r = as.numeric(r),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            er = as.numeric(er),
            ip = as.numeric(ip),
            era = as.numeric(era),
            whip = as.numeric(round((bb + h) / ip, 2)),
            `h/9` = as.numeric(round((h/ip)*9, 2)),
            `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2),
            img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')
          ) %>% 
          filter(ip > 50) %>% 
          arrange(`bb/9`) %>%
          select(years, first_name, last_name, `bb/9`, img) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, `bb/9`) %>%
          top_n(-10, `bb/9`) %>%
          rename(
            Año = years,
            Jugador = jugador,
            `BB/9` = `bb/9`,
            " " = img
          ) %>% 
          slice(1:(n()-1)) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          bb_9,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            
            # fixedColumns = list(LeftColumns = 3),
            # columnDefs = list(list(className = "dt-center", targets = 0)),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table picheo lideres so/bb ----
      output$pt_p_sobb <- renderDataTable({
        
        # Data ----
        so_bb <- prs() %>% 
          filter(ronda == "regular") %>%
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
          select(player_id, 1:28) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name),
                    by = c("player_id", "years")) %>%
          select(player_id, first_name,last_name, jugador, 2:29) %>%
          mutate(
            # edad = as.numeric(edad),
            w = as.numeric(w),
            l = as.numeric(l),
            g = as.numeric(g),
            gs = as.numeric(gs),
            cg = as.numeric(cg),
            sho = as.numeric(sho),
            sv = as.numeric(sv),
            h = as.numeric(h),
            r = as.numeric(r),
            hr = as.numeric(hr),
            bb = as.numeric(bb),
            so = as.numeric(so),
            er = as.numeric(er),
            ip = as.numeric(ip),
            era = as.numeric(era),
            whip = as.numeric(round((bb + h) / ip, 2)),
            `h/9` = as.numeric(round((h/ip)*9, 2)),
            `hr/9` = as.numeric(round((hr/ip)*9, 2)),
            `bb/9` = as.numeric(round((bb/ip)*9, 2)),
            `so/9` = as.numeric(round((so/ip)*9, 2)),
            `so/bb` = round(so / bb, 2)
          ) %>% 
          filter(ip > 50) %>% 
          arrange(desc(`so/bb`)) %>%
          select(years, first_name, last_name, `so/bb`) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>%
          select(img, jugador, years, `so/bb`) %>%
          top_n(10, `so/bb`) %>%
          rename(
            Año = years,
            Jugador = jugador,
            `SO/BB` = `so/bb`,
            " " = img
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          so_bb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 50 ip')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        )
      })
      
      #Batting Records by season 
      # Batting Record by season ----
      # Table bateo lideres H ----
      output$pt_b_hits <- renderDataTable({
        
        # Data ----
        hits <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(h)) %>%
          select(img, years, first_name, last_name, h) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, h) %>% 
          top_n(10, h) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            H = h,
            " " = img
          ) %>% 
          slice(1:(n()-3)) 
          
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hits,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),

            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table picheo lideres 2b ----
      output$pt_b_2b <- renderDataTable({
        
        # Data ----
        dobles <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(`2b`)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, `2b`) %>% 
          top_n(10, `2b`) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            `2B` = `2b`,
            " " = img
          ) %>% 
          slice(1:(n()- 2)) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          dobles,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      
      # Table bateo lideres 3B ----
      output$pt_b_3b <- renderDataTable({
        
        # Data ----
        triples <- brs() %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(`3b`)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, `3b`) %>% 
          top_n(10, `3b`) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            `3B` = `3b`,
             " " = img
          ) %>% 
          slice(1:(n()-2))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          triples,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres HR ----
      output$pt_b_hr <- renderDataTable({
        
        # Data ----
        hr <- brs() %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(hr)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, hr) %>% 
          top_n(10, hr) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            HR = hr,
            " " = img
          )
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          hr,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres AVG ----
      output$pt_b_average <- renderDataTable({
        
        # Data ----
        avg <- brs() %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          filter(ab >= 190) %>% 
          arrange(desc(avg)) %>% 
          mutate(avg = round(((h)/ ab), 3)) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador, years, avg) %>% 
          top_n(10, avg) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            AVG = avg, 
            " " = img
          ) %>% 
          arrange(desc(AVG)) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          avg,
          escape = FALSE,
          extensions = "ColReorder",
          # rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 190 AB')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres RBI ----
      output$pt_b_rbi <- renderDataTable({
        
        # Data ---- 
        rbi <- brs() %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(rbi)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, rbi) %>% 
          select(img, jugador, years, rbi) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            RBI = rbi,
            " " = img
          ) %>% 
          slice(1:(n()- 1)) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          rbi,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres SLG ----
      output$pt_b_slg <- renderDataTable({
        
        # Data ----
        slg <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          filter(years %in% temporadas_batting) %>% 
          mutate(slg = round(tb / ab, 3)) %>% 
          filter(ab >= 190) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, slg) %>% 
          select(img, jugador, years,slg) %>% 
          arrange(desc(slg)) %>%
          rename(
            Año = years,
            Jugador = jugador,
            SLG = slg, 
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          slg,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 190 BA')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres OBP ----
      output$pt_b_obp <- renderDataTable({
        
        # Data ----
        obp <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          filter(years %in% temporadas_batting) %>% 
          mutate(obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3)) %>% 
          filter(ab >= 190) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, obp) %>% 
          select(img, jugador, years, obp) %>% 
          arrange(desc(obp)) %>%
          rename(
            Año = years,
            Jugador = jugador,
            OBP = obp,
            " " = img
          ) 

        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          obp,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 190 BA')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres OPS ----
      output$pt_b_ops <- renderDataTable({
        
        # Data ----
        ops <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          filter(years %in% temporadas_batting) %>% 
          mutate(obp = round((h + bb + hbp) / (ab + bb + hbp + sf), 3),
                 slg = round(tb / ab, 3),
                 ops = obp + slg) %>% 
          filter(ab >= 190) %>% 
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, ops) %>% 
          select(img, jugador, years, ops) %>% 
          arrange(desc(ops)) %>%
          rename(
            Año = years,
            Jugador = jugador,
            OPS = ops,
            " " = img
          ) 
        

        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ops,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Con más de 190 BA')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres AB ----
      output$pt_b_ab <- renderDataTable({
        
        # Data ----
        ab <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
            mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(ab)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          select(img, jugador,years,  ab) %>% 
          top_n(10, ab) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            AB = ab,
            " " = img
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          ab,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres SB ----
      output$pt_b_sb <- renderDataTable({
        
        # Data ----
        sb <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(sb)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, sb) %>% 
          select(img, jugador, years, sb) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            SB = sb,
            " " = img
          ) %>% 
          slice(1:(n()- 1))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          sb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      # Table bateo lideres XB ----
      output$pt_b_xb <- renderDataTable({
        
        # Data ----
        xb <-brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador, sep = "")) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador, sep = "")) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(img = c('<img src="https://sports.cbsimg.net/images/baseball/mlb/players/25x35/2829944.jpg" height="20"></img>')) %>% 
          arrange(desc(xb)) %>%
          tidyr::unite('jugador', first_name, last_name, sep = ' ') %>% 
          top_n(10, xb) %>% 
          select(img, jugador, years, xb) %>% 
          rename(
            Año = years,
            Jugador = jugador,
            XB = xb
          ) %>% 
          slice(1:(n()- 6))
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          xb,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE,
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            # rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:3))),
            headerCallback = JS(headerCallback),
            # rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}"),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '10px'});",
              "$(this.api().table().header()).css({'font-size': '14px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
      })
      #LVBP records ----
      # Table Records LVBP  ----
      output$lvbp_general <- renderDataTable({

        # Data ----
        banderas <- c(
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #1
          '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>', #2
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #3
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #4
          '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>', #5
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #6
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #7
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #8
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #9
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #10
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #11 
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #12
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #13
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #14
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #15
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #16
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #17
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #18
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #19
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #20
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #21
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #22
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #23
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #24
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #25
          '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>', #26
          '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>', #27
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #28
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #29
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #30
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #31
          '<img src="https://upload.wikimedia.org/wikipedia/commons/a/a4/Flag_of_the_United_States.svg" width="25" height="15"></img>', #32 
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>', #33
          '<img src="https://upload.wikimedia.org/wikipedia/commons/0/06/Flag_of_Venezuela.svg" width="25" height="15"></img>' #34
        )
        
        crown <- rep(1, length(banderas))
        crown[[32]] <- '<img src="https://png.pngtree.com/png-clipart/20190903/original/pngtree-crown-icon-png-image_4422282.jpg" width="25" height="25"></img>'
        crown[[33]] <- '<img src="https://png.pngtree.com/png-clipart/20190903/original/pngtree-crown-icon-png-image_4422282.jpg" width="25" height="25"></img>'
        
        
        novatos <- lvbp() %>% 
          janitor::clean_names() %>% 
          mutate(
            flag = banderas,
            crown = if_else(crown == 1, " ", crown)
            ) %>%
          select(flag, jugador, years, premio, crown) %>% 
          rename(
            Año = years,
            Premio = premio,
            Jugador = jugador,
            " " = flag,
            "  " = crown
          )
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          novatos,
          escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          # caption = htmltools::tags$caption(
          #   style = 'caption-side: bottom; text-align: center;'
          #   , htmltools::em('Top 10 historico')),
          options = list(
            autoWidth = FALSE,
            ordering = FALSE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 0:3)),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
      })
      #Vzla vs importados ----
      # Table pitching vzla vs importado totales ----
      output$versus_total_pit <- renderDataTable({
        
        # Data ----
        versus_totales <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, 
                             estado, ciudad), by = 'key') %>%
          select(-player_id, ID, key, first_name,last_name, jugador, 2:35) %>% 
          mutate(
            importados =
              case_when(
                pais == "Venezuela" ~ "Venezolanos",
                pais %in% .paises_pitching[-c(9, 21)] ~ " Importados",
                TRUE ~ "Desconocido"
                )
            ) %>% 
          # filter(importados %in% c("Importados", "Venezolanos")) %>% 
          select(-edad) %>% 
          group_by(importados) %>% 
          summarise(
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(so/bb, 2)),
            .groups = "drop"
          ) %>%
          rename(
            Grupo = importados,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
            ) %>% 
          select(-G)
        
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_totales ,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores que no tienen información de su país y se asigna "Desconocidos"')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(1:20))
                              ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
     
      })
      # Table pitching vzla vs importado  Regular season ----
      output$versus_pit <- renderDataTable({
        
        # Data ----
        versus_pit_rs <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          mutate(importados =
                   case_when(
                     pais %in% .paises_pitching[-c(9, 21)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          )  %>% 
          select(-edad) %>% 
          group_by(importados, years) %>% 
          summarise(
            importados = last(importados),
            years = last(years),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(so/bb, 2)),
            .groups = "drop"
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Grupo = importados,
            Temporada = years, 
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) %>% 
          select(-G) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_pit_rs ,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores que no tienen información de su país y se asigna "Desconocidos"')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:21))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
     
      })
      # Table pitching vzla vs importado round robin ----
      output$versus_rr_pit <- renderDataTable({
        
        # Data ----
        versus_pit_rr <- prs() %>% 
          filter(ronda == "round robin") %>%
          select(player_id, 1:31) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name, pais),
                    by = c("player_id", "years")) %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_pitching[-c(9, 21)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          )  %>% 
          group_by(importados, years) %>% 
          summarise(
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(so/bb, 2)),
            .groups = "drop"
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Grupo = importados,
            Temporada = years, 
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) %>% 
          select(-G)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_pit_rr ,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores que no tienen información de su país y se asigna "Desconocidos"')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:21))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
     
      })
      # Table pitching vzla vs importado final ----
      output$versus_final_pit <- renderDataTable({
        
        # Data ----
        versus_final_pit <- prs() %>% 
          filter(ronda == "finales") %>%
          select(player_id, 1:31) %>%
          left_join(Rosters() %>%
                      select(player_id, years, name, ID, first_name, last_name, pais),
                    by = c("player_id", "years")) %>% 
          mutate(
            importados =
              case_when(
                pais %in% .paises_pitching[-c(9, 21)] ~ " Importados",
                pais == "Venezuela" ~ "Venezolanos",
                TRUE ~ "Desconocido"
                )
            )  %>% 
          group_by(importados, years) %>% 
          summarise(
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            whip = round((bb + h)/ ip, 2),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(so/bb, 2)),
            .groups = "drop"
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Grupo = importados,
            Temporada = years, 
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          ) %>% 
          select(-G)
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_final_pit,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Existen jugadores que no tienen información de su país y se asigna "Desconocidos"')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            # fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:21))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
     
      })
      # Table batting vzla vs importado totales ----
      output$versus_total_bat <- renderDataTable({
        
        # Data ----
        versus_total_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35, -player_id) %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_batting[-c(8, 17)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          ) %>% 
          group_by(importados) %>% 
          summarise(
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          # arrange(desc(years)) %>% 
          rename(
            Grupo = importados,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `TB` = tb,
            `XB` = xb,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_total_bat,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;', 
                      htmltools::em('Las estadísticas PA, CS, BB, SO, OBP, OPS, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
      })
      
      # Table batting vzla vs importado  ----
      output$versus_bat <- renderDataTable({
        
        # Data ----
        versus_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35, -player_id) %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_batting[-c(8, 17)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          )  %>%
          group_by(importados, years) %>% 
          summarise(
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Temporada = years,
            Grupo = importados,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_bat,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadísticas PA, CS, BB, SO, OBP, OPS, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
      })
      
      # Table batting vzla vs importado round robin ----
      output$versus_rr_bat <- renderDataTable({
        
        # Data ----
        versus_bat <- brs() %>% 
          filter(ronda == "round robin") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35, -player_id) %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_batting[-c(8, 17)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          )  %>%
          group_by(importados, years) %>% 
          summarise(
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Temporada = years,
            Grupo = importados,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          versus_bat,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadísticas PA, CS, BB, SO, OBP, OPS, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:22))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
      })
      
      # Table batting vzla vs importado finals ----
      output$versus_final_bat <- renderDataTable({
        
        # Data ----
        versus_bat_final <- brs() %>% 
          filter(ronda == "finales") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35, -player_id) %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_batting[-c(8, 17)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   )
          )  %>%
          group_by(importados, years) %>% 
          summarise(
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            avg = round(h/ab, 3),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            # ir = sum(ir, na.rm = T),
            # rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(desc(years)) %>% 
          rename(
            Temporada = years,
            Grupo = importados,
            `G` = g,
            `PA` = pa,
            `AB` = ab,
            `R` = r,
            `H` = h,
            `2B` = '2b',
            `3B` = '3b',
            `HR` = hr,
            `RBI` = rbi,
            `SB` = sb,
            `CS` = cs,
            `BB` = bb,
            `SO` = so,
            `AVG` = avg,
            `OBP` = obp,
            `SLG` = slg,
            `OPS` = ops,
            # `RC` = rc,
            `TB` = tb,
            `XB` = xb,
            # `IR` = ir,
            `HBP` = hbp,
            `SH` = sh,
            `SF` = sf
          ) 
        
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        
        DT::datatable(
          versus_bat_final,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadísticas PA, CS, BB, SO, OBP, OPS, RC, HBP, SH y SF 
                            son registradas desde la temporada 2005-06')),
          options = list(
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            autoWidth = TRUE,
            searching = TRUE,
            paging = TRUE,
            pageLegth = 25,
            lengthMenu = c(25, 20, 100),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:23))),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
        
      })
      
      # Demographics -----
      # Table Geo Pitching stats by city ----
      output$city_pit <- renderDataTable({
        req(input$select_country)
        
        geographic <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          select(-edad) %>% 
          group_by(pais, estado, ciudad) %>% 
          summarise(
            jugador = n(),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            .groups = "drop"
          ) %>% 
          arrange(desc(pais)) %>% 
          filter(pais == input$select_country)
        
        
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To deleate header line horizontal in bottom of colums name
        
        DT::datatable(
          geographic ,
          # escape = FALSE,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Top 10 historico')),
          options = list(
            ordering = F, # To delete Ordering
            dom = 'ft',  # To remove showing 1 to n of entries fields
            # autoWidth = TRUE
            searching = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = 1),
                              list(width = '120px', targets = 0)),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) 
     
      })
      # Table Geo Pitching stats by state ----
      output$state_pit <- renderDataTable({
        
        pais_pit <- prs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>% 
          group_by(pais, estado) %>% 
          summarise(
            jugador = n(),
            w = sum(w, na.rm = T),
            l = sum(l, na.rm = T),
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = as.character(round((er * 9) / ip, 2)),
            g = sum(g, na.rm = T),
            gs = sum(gs, na.rm = T),
            cg = sum(cg, na.rm = T),
            sho = sum(sho, na.rm = T),
            sv = sum(sv, na.rm = T),
            h = sum(h, na.rm = T),
            r = sum(r, na.rm = T),
            hr = sum(hr, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            ir = sum(ir, na.rm = T),
            whip = as.character(round(mean(whip, na.rm = T), 2)),
            `h/9` = as.character(round((h/ip)*9, 2)),
            `hr/9` = as.character(round((hr/ip)*9, 2)),
            `bb/9` = as.character(round((bb/ip)*9, 2)),
            `so/9` = as.character(round((so/ip)*9, 2)),
            `so/bb` = as.character(round(mean(`so/bb`, na.rm = T), 2)),
            .groups = "drop"
          ) %>% 
          arrange(pais, estado)
        
        
        
      })
      # Table Geo Batting stats by state ----
      output$state_bat <- renderDataTable({
        
        state_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          group_by(pais, estado) %>% 
          summarise(
            jugador = n(),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(pais, desc(jugador))
        
      })
      # Table Geo Batting stats by city ----
      output$city_bat <- renderDataTable({
        
        city_bat <- brs() %>% 
          filter(ronda == "regular") %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:35) %>%
          group_by(pais, estado, ciudad) %>% 
          summarise(
            jugador = n(),
            g = sum(g, na.rm = T),
            pa = sum(pa, na.rm = T),
            ab = sum(ab, na.rm = T),
            r = sum(r, na.rm = T),
            h = sum(h, na.rm = T),
            `2b` = sum(`2b`, na.rm = T),
            `3b` = sum(`3b`, na.rm = T),
            hr = sum(hr, na.rm = T),
            rbi = sum(rbi, na.rm = T),
            sb = sum(sb, na.rm = T),
            cs = sum(cs, na.rm = T),
            bb = sum(bb, na.rm = T),
            so = sum(so, na.rm = T),
            obp = round(sum(h, bb, hbp, na.rm = T) / sum(ab, bb, hbp, sf, na.rm = T), 3),
            slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
            ops = round(sum(slg, obp, na.rm = T), 3),
            ir = sum(ir, na.rm = T),
            rc = sum(rc, na.rm = T),
            tb = sum(tb, na.rm = T),
            xb = sum(xb, na.rm = T),
            hbp = sum(hbp, na.rm = T),
            sh = sum(sh, na.rm = T),
            sf = sum(sf, na.rm = T),
            .groups = 'drop'
          ) %>% 
          arrange(pais, desc(jugador))
        
      })
      # -----TABLES ----
      #Advanced Search
      # Table Pitching advanced ----
      observeEvent(input$btn_searh_pit, {
        
        output$advance_pitching <- renderDataTable({
        # Data ----
        pit_search <- prs() %>% 
          left_join(Rosters() %>% 
                      select(player_id, years, first_name, last_name, pos, bat, lan, exp, pais, estado, ciudad, f_nac,
                             ronda),
                    by = c("player_id", "years", "ronda")) %>% 
          mutate(
            from = as.numeric(str_sub(years, 1, 4)),
            to = from + 1
          ) %>% 
          filter(
            ronda == input$ronda,
            # ronda == "regular",
            to == as.numeric(str_sub(input$hasta_p, 1, 4)),
            # to <= as.numeric(str_sub("2010-11", 1, 4)),
            from == as.numeric(str_sub(input$desde_p, 1, 4)),
            # from >= as.numeric(str_sub("1970-71", 1, 4)),
            # input$Id086 >= as.numeric(input$criteria_1)
            w >= input$criteria_1
          ) %>% 
          arrange(years, jugador) %>% 
          select(2:27) %>% 
          rename(
            Jugador = jugador,
            `Temporada` = years,
            `Edad` = edad,
            `W` = w,
            `L` = l,
            `ERA` = era,
            `G` = g,
            `GS` = gs,
            `CG` = cg,
            `SHO` = sho,
            `SV` = sv,
            `IP` = ip,
            `H` = h,
            `R` = r,
            `ER` = er,
            `HR` = hr,
            `BB` = bb,
            `SO` = so,
            `WHIP` = whip,
            `H/9` = `h/9`,
            `HR/9` = `hr/9`,
            `BB/9` = `bb/9`,
            `SO/9` = `so/9`,
            `SO/BB` = `so/bb`
          )
      
          
    
        # Table ----
        headerCallback <- c(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('border-bottom', 'none');",
          "}"
        )  # To delete header line horizontal in bottom of columns name
        
        DT::datatable(
          pit_search,
          extensions = "ColReorder",
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;'
            , htmltools::em('Las estadísticas SHO, R, HR y HR/9
                            son registradas desde la temporada 2005-06')),
          options = list(
            autoWidth = TRUE,
            # dom = 'ft',  # To remove showing 1 to n of entries fields
            searching = FALSE,
            paging = TRUE,
            pageLegth = 20,
            lengthMenu = c(20, 50, 70),
            lengthChange = FALSE,
            scrollX = TRUE,
            rownames = FALSE,
            fixedHeader = TRUE,
            fixedColumns = list(LeftColumns = 3),
            columnDefs = list(list(className = "dt-center", targets = c(0:25))
                              # list(width = '100px', targets = 1)
            ),
            headerCallback = JS(headerCallback),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-family': 'Calibri'});",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px', 'font-family': 'Courier'});",
              "}"
            )
          )
        ) %>% 
          formatStyle(
            'Temporada',
            target = "row",
            fontWeight = styleEqual(c('Total'), "bold")
          )
        })
      })
      
       # Info Boxes ----
      # InfoBox Position player ----
      output$pos <- renderInfoBox({
        
        pos_player <- info_player() %>% 
          select(pos) %>%
          summarise(
            pos = last(pos))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('Posición',
                value = pos_player,
                color = 'blue',
                icon = icon('baseball', lib = 'font-awesome')
        )
      })
      
      # InfoBox lan player ----
      output$lan <- renderInfoBox({
        
        lan_player <- info_player() %>% 
          select(lan) %>%
          summarise(
            lan = last(lan))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('Lanza',
                value = lan_player,
                color = 'blue',
                icon = icon('baseball-ball', lib = 'font-awesome')
        )
      })
      
      # InfoBox bat player ----
      output$bat <- renderInfoBox({
        
        bat_player <- info_player() %>% 
          select(bat) %>%
          summarise(
            bat = last(bat))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('Batea',
                value = bat_player,
                color = 'blue',
                icon = icon('usd', lib = 'glyphicon')
        )
      })
      
      
      
      
      
      
      # InfoBox pais player ----
      output$pais <- renderInfoBox({
        
        pais_player <- info_player() %>% 
          select(pais) %>%
          summarise(
            pais = last(pais))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('País',
                value = pais_player,
                color = 'red',
                icon = icon('globe-americas', lib = 'font-awesome')
        )
      })
      
      # InfoBox estado player ----
      output$estado <- renderInfoBox({
        
        estado_player <- info_player() %>% 
          select(estado) %>%
          summarise(
            estado = last(estado))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('Estado',
                value = estado_player,
                color = 'red',
                icon = icon('map-marker', lib = 'font-awesome')
        )
      })
      
      # InfoBox ciudad player ----
      output$ciudad <- renderInfoBox({
        
        ciudad_player <- info_player() %>% 
          select(ciudad) %>%
          summarise(
            ciudad = last(ciudad))
        
        # box_color = if_else(net_profit >= 0, 'green', 'red')
        
        infoBox('Ciudad',
                value = ciudad_player,
                color = 'red',
                icon = icon('building', lib = 'font-awesome')
        )
      })
      
      
      
      # Images
      
      
      # ---- VALUEBOX ----
      # Valuebox pitching countrys ----
      output$valuebox_cpit <- renderbs4ValueBox({
        
        .country_pit <- prs() %>% 
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name, pais, estado, ciudad), by = 'key') %>%
          select(pais) %>% 
          filter(!is.na(pais),
                 !pais == "Desconocido") %>% 
          summarise(pais = unique(pais)) %>% 
          pull() 
          
        
        bs4ValueBox(
          value = tags$p("Paises", style = "font-size: 120%;
                                                   font-family:Comic Sans;"),
          # subtitle = ,
          subtitle = tags$h2(length(.country_pit), style = "font-size: 170%;"),
          status = "primary"
          # icon = 'check-circle'
          # href = "https://www.worldometers.info/coronavirus/"
        )
        
      })
      
      # Valuebox pitching pitchers ----
      output$valuebox_lan <- renderbs4ValueBox({
        
        country_pit <- Rosters() %>% 
          filter(pos == "P",
                 !is.na(lan)) %>% 
          select(lan, ID) %>% 
          mutate(derechos = if_else(lan == "D", 1, 0),
                 zurdos = if_else(lan == "Z", 1, 0),
                 lan = derechos + zurdos) %>% 
          group_by(ID) %>% 
          summarise(
            .groups = "drop",
            derechos = sum(last(derechos)),
            zurdos = sum(last(zurdos)),
            lan = sum(last(lan))
          ) %>% 
          summarise(
            derechos = sum(derechos),
            zurdos = sum(zurdos),
            lan = sum(lan)
          )
          
        
        bs4ValueBox(
          value = tags$p("Lanzadores", style = "font-size: 120%;
                                                   font-family:Comic Sans;"),
          # subtitle = ,
          subtitle = tags$h2(country_pit$lan, style = "font-size: 170%;"),
          status = "primary"
          # icon = 'check-circle'
          # href = "https://www.worldometers.info/coronavirus/"
        )
        
      })
      
      # Valuebox pitching pitchers right ----
      output$valuebox_right <- renderbs4ValueBox({
        
        country_pit <- Rosters() %>% 
          filter(pos == "P",
                 !is.na(lan)) %>% 
          select(lan, ID) %>% 
          mutate(derechos = if_else(lan == "D", 1, 0),
                 zurdos = if_else(lan == "Z", 1, 0),
                 lan = derechos + zurdos) %>% 
          group_by(ID) %>% 
          summarise(
            .groups = "drop",
            derechos = sum(last(derechos)),
            zurdos = sum(last(zurdos)),
            lan = sum(last(lan))
          ) %>% 
          summarise(
            derechos = sum(derechos),
            zurdos = sum(zurdos),
            lan = sum(lan)
          )
          
        
        bs4ValueBox(
          value = tags$p("Lanzadores derechos", style = "font-size: 120%;
                                                   font-family:Comic Sans;"),
          # subtitle = ,
          subtitle = tags$h2(country_pit$derechos, style = "font-size: 170%;"),
          status = "primary"
          # icon = 'check-circle'
          # href = "https://www.worldometers.info/coronavirus/"
        )
        
      })
      
      # Valuebox pitching pitchers left ----
      output$valuebox_left <- renderbs4ValueBox({
        
        country_pit <- Rosters() %>% 
          filter(pos == "P",
                 !is.na(lan)) %>% 
          select(lan, ID) %>% 
          mutate(derechos = if_else(lan == "D", 1, 0),
                 zurdos = if_else(lan == "Z", 1, 0),
                 lan = derechos + zurdos) %>% 
          group_by(ID) %>% 
          summarise(
            .groups = "drop",
            derechos = sum(last(derechos)),
            zurdos = sum(last(zurdos)),
            lan = sum(last(lan))
          ) %>% 
          summarise(
            derechos = sum(derechos),
            zurdos = sum(zurdos),
            lan = sum(lan))
          

        bs4ValueBox(
          value = tags$p("Lanzadores zurdos", style = "font-size: 120%;
                                                   font-family:Comic Sans;"),
          # subtitle = ,
          subtitle = tags$h2(country_pit$zurdos, style = "font-size: 170%;"),
          status = "primary"
          # icon = 'check-circle'
          # href = "https://www.worldometers.info/coronavirus/"
        )
        
      })
      
      # ValueBox ERA ----
      output$era <- renderValueBox({
        req(input$select_jugador_pit)
        
        
        era <- prs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_pit) %>% 
          summarise(
            er = sum(er, na.rm = T),
            ip = IP(ip),
            era = round((er * 9) / ip, 2),
            .groups = 'drop'
          ) 
          
        
        if (era$er == 0 & era$ip == 0) {
          era <- "-"
        }
        else if (is.numeric(era$er)) {
          era <- era %>% 
            select(era) %>% 
            pull()
        }
        
        
        shinydashboard::valueBox(
          value = tags$h2("ERA", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h1(era, style = 'font-size: 23px;
                                           font-weight: 700;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox G-P ----
      output$g_p <- renderValueBox({
        req(input$select_jugador_pit)
        
        
        w <- prs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>% 
          summarise(
            w = as.character(sum(w, na.rm = T)),
            .groups = 'drop'
          ) %>% 
          select(w) %>% 
          pull()
        
        
        l <- prs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>% 
          summarise(
            l = as.character(sum(l, na.rm = T)),
            .groups = 'drop'
          ) %>% 
          select(l) %>% 
          pull()
        
        
        shinydashboard::valueBox(
          value = tags$h2("W-L", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(paste0(w, "-", l), 
                                  style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox K ----
      output$k <- renderValueBox({
        req(input$select_jugador_pit)
        
        so <- prs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>% 
          summarise(
            so = as.character(sum(so, na.rm = T)),
            .groups = 'drop'
          ) %>% 
          select(so) %>% 
          pull()
        
        
        shinydashboard::valueBox(
          value = tags$h2("K", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(so, style = ' font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox WHIP ----
      output$whip <- renderValueBox({
        req(input$select_jugador_pit)
        
        whip <- prs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:27) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:29) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) 
        
        if (length(whip$ip) == 0) {
          whip <- "-"
        }
        else if (is.numeric(whip$ip)) {
          whip <- whip %>% 
            summarise(
              h = sum(h, na.rm = T),
              bb = sum(bb, na.rm = T),
              ip = IP(ip),
              whip = round(sum(h, bb) / ip, 2),
              .groups = 'drop'
            ) %>% 
            select(whip) %>% 
            pull()
        }
        

        
        shinydashboard::valueBox(
          value = tags$h2("WHIP", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(whip, style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox AVG ----
      output$avg_b <- renderValueBox({
        req(input$select_jugador_bat)
        
       avg <- brs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:32) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_bat) 
      # "Raúl Pérez Tovar"
        
        if (length(avg$avg) == 0) {
          avg <- "-"
        }
        else if (is.numeric(avg$avg)) {
          avg <- avg %>% 
            summarise(
              avg = round(sum(h)/sum(ab), 3),
              .groups = 'drop'
            ) %>% 
            select(avg) %>% 
            pull()
        }
        
        
        
        shinydashboard::valueBox(
          value = tags$h2("AVG", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(avg, style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox HR ----
      output$hr_b <- renderValueBox({
        req(input$select_jugador_bat)
        
       hr <- brs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:32) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_bat) %>% 
         summarise(
           hr = sum(hr),
           .groups = 'drop'
         ) %>% 
         select(hr) %>% 
         pull()

        
        shinydashboard::valueBox(
          value = tags$h2("HR", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(hr, style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox RBI ----
      output$rbi_b <- renderValueBox({
        req(input$select_jugador_bat)
        
       rbi <- brs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:32) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_bat) %>% 
         summarise(
           rbi = sum(rbi),
           .groups = 'drop'
         ) %>% 
         select(rbi) %>% 
         pull()

        
        shinydashboard::valueBox(
          value = tags$h2("RBI", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(rbi, style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ValueBox BB ----
      output$slg_b <- renderValueBox({
        req(input$select_jugador_bat)
        
       slg <- brs() %>%
          mutate(key = paste0(as.character(years), jugador)) %>% 
          select(key, 1:28) %>% 
          left_join(Rosters() %>%
                      mutate(key = paste0(as.character(years), jugador)) %>%
                      select(key, name, ID, first_name, last_name), by = 'key') %>%
          select(ID, key, first_name,last_name, jugador, 2:32) %>%
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_bat) %>% 
         summarise(
           ab = sum(ab),
           h = sum(h),
           `2b` = sum(`2b`),
           `3b` = sum(`3b`),
           hr = sum(hr),
           slg = round(sum(h - `2b` - `3b` - hr, (2 *`2b`), (3 * `3b`), (4 * hr), na.rm = T) / ab, 3),
           .groups = 'drop'
         ) %>% 
         select(slg) %>% 
         pull()

        
        shinydashboard::valueBox(
          value = tags$h2("SLG", style = 'color: #6c6d6f; 
                                          font-size: 10px;
                                          font-weight: 700;
                                          font-family: "Roboto Regular", sans-serif;
                                          text-align: center!important;'),
          subtitle = tags$h2(slg, style = 'font-size: 23px;
                                           font-weight: 800;
                                           font-family: "Roboto Regular", sans-serif;
                                           text-align: center!important;
                                            ')
        )
        
      })
      # ---- CHARTS-----
      # Chart country distribution ----
      output$country_chart <- renderHighchart({
        
        country_bat <- Rosters() %>%
          mutate(importados =
                   case_when(
                     pais %in% .paises_pitching[-c(8, 18)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   ),
                 bateadores = if_else(
                   pos %in% c("CF", "IF", "2B", "1B", "3B", "RF", "LF", "C", 
                              "SS", "BD", "BE"), "Bateadores", "Lanzadores"
                 )
                 
          ) %>% 
          group_by(pais, bateadores) %>% 
          summarise(
            importados = n(),
            .groups = 'drop'
          ) %>% 
          arrange(desc(importados)) %>% 
          filter(!is.na(pais)) %>% 
          hchart(.,
                 type = 'bar', 
                 hcaes(x = pais,
                       y = importados,
                       group = bateadores
                       # color = bateadores
                       # color = c("#0D3583", "4A4A4B")
                       )) %>% 
          hc_title(
            text = "<span style=\"color:#b90e13; 
                      text-shadow: 1px 1px 2px rgba(150, 150, 150, 1)\">GUAIRISTAS por paises</span>",
            useHTML = TRUE) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          # hc_tooltip(sort = TRUE,
          #            pointFormat = paste('<br><b>Jugadores: {point.importados:1f}')) %>%
          # hc_chart(backgroundColor = "black") %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
 
        country_bat
        
      })
      # Chart foreign distribution  ----
      output$foreign_chart <- renderHighchart({

        foreign_chart <- Rosters() %>%
          mutate(
            importados =
                   case_when(
                     pais %in% .paises_pitching[-c(8, 18)] ~ " Importados",
                     pais == "Venezuela" ~ "Venezolanos",
                     TRUE ~ "Desconocido"
                   ),
                 bateadores = if_else(
                   pos %in% c("CF", "IF", "2B", "1B", "3B", "RF", "LF", "C", 
                              "SS", "BD", "BE"), "Bateadores", "Lanzadores"
                 )
                 
          ) %>% 
          ungroup() %>% 
          group_by(importados) %>% 
          summarise(
            position = n(),
            .groups = 'drop'
          ) %>% 
          arrange(desc(importados)) %>% 
          hchart(.,
                 type = 'column', 
                 hcaes(x = importados,
                       y = position,
                       # group = importados,
                       color = c("#011C51", '#C20B10', "4A4A4B")
                       # color = c("#011C51", '#C20B10', )
                 )) %>% 
          hc_title(
            text = "<span style=\"color:#b90e13; 
                      text-shadow: 1px 1px 2px rgba(150, 150, 150, 1)\">VENEZOLANOS VS IMPORTADOS</span>",
            useHTML = TRUE) %>%
          hc_plotOptions(series = list(stacking = "normal")) %>%
          hc_tooltip(sort = TRUE,
                     pointFormat = paste('<br><b>Jugadores: {point.position:1f}')) %>%
          # hc_chart(backgroundColor = "black") %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
        
        foreign_chart
        
      })
      
      # Chart position distribution  ----
      output$position_chart <- renderHighchart({

        position_chart <- Rosters() %>%
          mutate(
            importados =
              case_when(
                pais %in% .paises_pitching[-c(8, 18)] ~ " Importados",
                pais == "Venezuela" ~ "Venezolanos",
                TRUE ~ "Desconocido"
              ),
            bateadores = if_else(
              pos %in% c("CF", "IF", "2B", "1B", "3B", "RF", "LF", "C", 
                         "SS", "BD", "BE"), "Bateadores", "Lanzadores"
            )
          ) %>% 
          ungroup() %>% 
          group_by(bateadores) %>% 
          summarise(
            importados = n(),
            .groups = 'drop'
          ) %>% 
          hchart(.,
                 type = 'column', 
                 hcaes(x = bateadores,
                       y = importados,
                       # group = position
                       color = c("#011C51", '#C20B10')
                 )) %>% 
          hc_title(
            text = "<span style=\"color:#b90e13; 
                      text-shadow: 1px 1px 2px rgba(150, 150, 150, 1)\">BATEADORES vs LANZADORES</span>",
            useHTML = TRUE) %>%
          hc_plotOptions(series = list(stacking = "normal")) %>%
          hc_tooltip(sort = TRUE,
                     pointFormat = paste('<br><b>Jugadores: {point.importados:1f}')) %>%
          # hc_chart(backgroundColor = "black") %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
        
        position_chart
        
      })
      
      # Chart country distribution by countries ----
      output$country_chart_paises <- renderHighchart({
        
        country_bat <- Rosters() %>%
          mutate(
            importados =
              case_when(
                pais %in% .paises_pitching[-c(8, 18)] ~ " Importados",
                pais == "Venezuela" ~ "Venezolanos",
                TRUE ~ "Desconocido"
              ),
            bateadores = if_else(
              pos %in% c("CF", "IF", "2B", "1B", "3B", "RF", "LF", "C", 
                         "SS", "BD", "BE"), "Bateadores", "Lanzadores"
            )
          ) %>% 
          ungroup() %>% 
          filter(pais == "Cuba") %>% 
          group_by(bateadores) %>% 
          summarise(
            importados = n(),
            .groups = 'drop'
          ) %>% 
          hchart(.,
                 type = 'bar', 
                 hcaes(x = bateadores,
                       y = importados,
                       # group = position
                       color = c("#011C51", '#C20B10')
                 )) %>% 
          hc_title(
            text = "<span style=\"color:#b90e13; 
                      text-shadow: 1px 1px 2px rgba(150, 150, 150, 1)\">GUAIRISTAS por paises</span>",
            useHTML = TRUE) %>%
          hc_plotOptions(column = list(stacking = "normal")) %>%
          # hc_tooltip(sort = TRUE,
          #            pointFormat = paste('<br><b>Jugadores: {point.importados:1f}')) %>%
          # hc_chart(backgroundColor = "black") %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = ""))
        
        country_bat
        
      })
      # -----IMAGE ----
      # image Pitching Player ----
      output$jugador_pit <- renderImage({
        # req(input$select_jugador_pit)
        
        player <- 'www/sombra_jugador.png'
        # player <- paste('www/pitching/',"A. Cardona",'.jpg', sep = '')
        logo <- "www/pitching/ts_isotipo.png"
        
        if (file.exists(player) == TRUE) {
          return(
            list(
              src = player,
              contenType = "image/png",
              width = 75,
              height = 100,
              align = "center"
            )
          )
        }
        
        else if (file.exists(player) == FALSE) {
          return(
            list(
              src = logo,
              contentType = "image/png",
              width = 75,
              height = 100,
              align = "center"
            )
          )
        }
        
      }, deleteFile = FALSE)
      
      
      
      # image Batting player ----
      output$jugador_bat <- renderImage({
        # req(input$select_jugador)
          
          player <- 'www/sombra_jugador.png'
          # player <- paste('www/pitching/',"A. Cardona",'.jpg', sep = '')
          logo <- "www/pitching/ts_isotipo.png"
          
          if (file.exists(player) == TRUE) {
            return(
              list(
                src = player,
                contenType = "image/png",
                width = 75,
                height = 100,
                align = "center"
              )
            )
          }
          
          else if (file.exists(player) == FALSE) {
            return(
              list(
                src = logo,
                contentType = "image/png",
                width = 75,
                height = 100,
                align = "center"
              )
            )
          }
          
        }, deleteFile = FALSE)
      
      
      # image Record AVG ----
      output$record_avg <- renderImage({
        req(input$select_jugador)
        
        player <- 'www/batting/C. Suarez.jpg'
        
        if (1 > 0) {
          return(list(
            src = 'http://www.milb.com/images/500235/t442/180x270/500235.jpg',
            contentType = "image/jpg",
            width = 150,
            height = 150
            # alt = 'Selecciona un jugador'
          ))
        }
      }, deleteFile = FALSE)
      
      # ----TEXT OUTPUT -----
      # Text output First name Pitcher ----
      output$first_name_pit <- renderText({
        req(input$select_jugador_pit)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_pit) %>% 
          #Before first space
          # mutate(player = stri_extract_first(player, regex = "\\w+")) %>% 
          select(player) %>% 
          unique() %>% 
          pull()
      })
      
      # Text output First name Hitter ----
      output$first_name_bat <- renderText({
        req(input$select_jugador_bat)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          #Before first space
          # mutate(player = stri_extract_first(player, regex = "\\w+")) %>% 
          select(player) %>% 
          unique() %>% 
          pull()
      })
      
      # Text output Last name Pitcher ----
      output$last_name_pit <- renderText({
        req(input$select_jugador_pit)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit
            ) %>% 
          # mutate(player = sub("^\\S+\\s+", '', player)) %>% 
          select(last_name) %>% 
          unique() %>% 
          pull()

      })
      # Text output Last name Hitter ----
      output$last_name_bat <- renderText({
        req(input$select_jugador_pit)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat
            ) %>% 
          # mutate(player = sub("^\\S+\\s+", '', player)) %>% 
          select(last_name) %>% 
          unique() %>% 
          pull()

      })
      # Text output Pitching Jugador batea-lanza ----
      output$B_L_L <- renderText({
        req(input$select_jugador_pit)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          select(bat) %>% 
          summarise(
            bat = last(bat),
            bat = if_else(bat == "D", "Derecha", if_else(bat == "A", "Ambidiestro", 
                                                         "Zurda")),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          select(lan) %>% 
          summarise(
            lan = last(lan),
            lan = if_else(lan == "D", "Derecha", "Zurda"),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        paste(" ", df, '/', df1, sep = ' ')
        
      })
      
      # Text output Batting Jugador batea-lanza ----
      output$B_L_B <- renderText({
        req(input$select_jugador_bat)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          select(bat) %>% 
          summarise(
            bat = last(bat),
            bat = if_else(bat == "D", "Derecha", if_else(bat == "A", "Ambidiestro", 
                                                         "Zurda")),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          select(lan) %>% 
          summarise(
            lan = last(lan),
            lan = if_else(lan == "D", "Derecha", "Zurda"),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        paste(" ", df, '/', df1, sep = ' ')
        
      })
      
      # Text output Batting Jugador position ----
      output$POS_B <- renderText({
        req(input$select_jugador_bat)
        
        df_h <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>%
            # player == "Rainer Olmedo") %>%
          summarise(
            hand = paste0(bat, "/", lan),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>%
            # player == "Rainer Olmedo") %>%
          summarise(
            pos = last(pos),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        
        # Final Result   
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>%
          # player == "Rainer Olmedo") %>%
          select(pais) %>% 
          summarise(
            pais = last(pais)
          ) %>% 
          unique() %>% 
          pull()
        
        df2 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_bat) %>%
          # filter(player == "Rainer Olmedo") %>%
          select(ciudad) %>% 
          summarise(
            estado = last(ciudad)
          ) %>% 
          unique() %>% 
          pull()
        
        if (is.na(df1) == TRUE) {
          paste0(df, "     • ", df_h)
          
        } else if (is.na(df1) == FALSE) {
          paste0(df1,', ', df2, "   •  ", df , "   •   ", "B/L:" , df_h)
        }
        
      })
      
      # Text output Pitching Jugador position ----
      output$POS_L <- renderText({
        req(input$select_jugador_pit)

        df_h <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          select(lan) %>% 
          summarise(
            hand = ifelse(lan == "D", "RHP", "LHP"),
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        if (is.na(df_h) == TRUE) {
          "P"
          
        } else if (is.na(df_h) == FALSE) {
          df_h
        }
        
        
     # Final Result   
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>%
            # player == "Junior Guerra") %>%
          select(pais) %>% 
          summarise(
            pais = last(pais)
          ) %>% 
          unique() %>% 
          pull()
        
        df2 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(player == input$select_jugador_pit) %>%
          # filter(player == "Junior Guerra") %>% 
          select(ciudad) %>% 
          summarise(
            estado = last(ciudad)
          ) %>% 
          unique() %>% 
          pull()
        
        if (is.na(df1) == TRUE) {
          paste0(df1, "     • ", df_h)
          
        } else if (is.na(df1) == FALSE) {
          paste0(df1,', ', df2, "   •  ", df_h)
        }
        
      })
      
      # Text output Pitching Lugar de nacimiento ----
      output$BORN <- renderText({
        req(input$select_jugador_pit)
        
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          select(pais) %>% 
          summarise(
            pais = last(pais)
          ) %>% 
          unique() %>% 
          pull()
        
        df1 <- Rosters() %>% 
          filter(jugador == input$select_jugador_pit) %>% 
          select(ciudad) %>% 
          summarise(
            estado = last(ciudad)
          ) %>% 
          unique() %>% 
          pull()
        
        if (is.na(df1) == TRUE) {
          paste0(df)
          
        } else if (is.na(df1) == FALSE) {
          paste0(df1,', ', df)
          
        }
        
      })
      
      # Text output Batting Lugar de nacimiento ----
      output$BORN_B <- renderText({
        req(input$select_jugador_bat)
        
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          select(pais) %>% 
          summarise(
            pais = last(pais)
          ) %>% 
          unique() %>% 
          pull()
        
        df1 <- Rosters() %>% 
          filter(jugador == input$select_jugador_bat) %>% 
          select(ciudad) %>% 
          summarise(
            estado = last(ciudad)
          ) %>% 
          unique() %>% 
          pull()
        
        if (is.na(df1) == TRUE) {
          paste0(df)
          
        } else if (is.na(df1) == FALSE) {
          paste0(df1,', ', df)
          
        }
        
      })
      
      # Text output Lugar de nacimiento ----
      output$BORN_PIT <- renderText({
        "Nacido:"
        
      })
      
      # Text output Pitching Experience ----
      output$EXP_L <- renderText({
        req(input$select_jugador_pit)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          summarise(
            exp = n(), 
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            pos == "P",
            player == input$select_jugador_pit) %>% 
          summarise(
            fecha = last(f_nac), 
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        
        paste("Nac:", df1, '    • Exp:', df, sep = ' ')
        
      })
      # Text output Batting Experience ----
      output$EXP_B <- renderText({
        req(input$select_jugador_bat)
        
        df <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          summarise(
            exp = n(), 
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        
        df1 <- Rosters() %>% 
          mutate(player = paste0(first_name, " ", last_name)) %>% 
          filter(
            player == input$select_jugador_bat) %>% 
          summarise(
            fecha = last(f_nac), 
            .groups = "drop"
          ) %>% 
          unique() %>% 
          pull()
        
        
        paste("Nac:", df1, '    • Exp:', df, sep = ' ')
        
      })

    }
  )


  

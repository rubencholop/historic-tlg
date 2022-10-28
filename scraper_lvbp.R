# libraries ----
library(dplyr)
library(rvest)
library(tidyverse)

url <- "https://www.lvbp.com/gameday.php?game=714290"

# Function Scraper ----
GetGameInfoLVBP <- function(URL){
  flog.info('Getting available URl')
  
  
  battinglogs <- httr::GET(rvest::read_html(url)) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    as.data.frame() %>% 
    select(-X1)
  
  
}


get_roster <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  roster <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    as.data.frame() %>% 
    select(-X1) %>% 
    rename(
      'jugador' = X2,
      'f.nacimiento' = X3,
      'name' = X4,
      'pos' = X5,
      'bat' = X6,
      'lan' = X7,
      'exp' = X8,
      'pais' = X9,
      'estado' = X10,
      'ciudad' = X11
    ) %>% 
    subset(jugador != 'Nombre') %>% 
    mutate(years = years) %>% 
    select(years, jugador, name, pos, bat, lan, exp, pais, estado, ciudad)
  flog.info('Data Wrangling completed')
  
  flog.info('Roster Df')
  
}

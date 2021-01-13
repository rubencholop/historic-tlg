# Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
library(progress)
library(stringr)
library(purrr)
library(tidyselect)
#page http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63

#P age year 1962-1963
.URL <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=2020-21'
str_extract(.url, '(?<=TIB&TE=).*')


# Roster players ----
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
  roster %>% 
    write_csv(path = paste0(years, "- roster.csv"))
}


get_roster(.URL)

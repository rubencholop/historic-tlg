# Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
library(progress)
library(stringr)
library(purrr)
library(tidyselect)
library(readr)
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

# Batting players ---- 
get_batting <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  batting <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[2]] %>% 
    as.data.frame() %>% 
    select(-X2) %>% 
    rename(
      'jugador' = X1,
      'edad' = X3,
      'g' = X4,
      'pa' = X5,
      'ab' = X6,
      'r' = X7,
      'h' = X8,
      '2b' = X9,
      '3b' = X10,
      'hr' = X11,
      'rbi' = X12,
      'sb' = X13,
      'cs' = X14,
      'bb' = X15,
      'so' = X16,
      'avg' = X17,
      'obp' = X18,
      'slg' = X19,
      'ops' = X20,
      'ir' = X21,
      'rc' = X22,
      'tb' = X23,
      'xb' = X24,
      'hbp' = X25,
      'sh' = X26,
      'sf' = X27
    ) %>% 
    subset(edad != 'EDAD') %>% 
    slice(1:(n()-3)
    ) %>% 
    mutate(years = years) 
  # %>% 
  # select(years, jugador, edad, g, pa, ab, r, h, '2b', '3b', hr, rbi, sb, cs, bb,s,
  #        oavg, obp, slg, ops, ir, rc, tb, xb, hbp, sh, sf)
  flog.info('Data Wrangling completed')
  
  
  flog.info('Batting Df')
  batting %>% 
    write_csv(path = paste0(years, "- batting.csv"))
}
get_batting(.URL)

# Pitching players ----
get_pitching <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'X2' = X2,
      'edad' = X3,
      'w' = X4,
      'l' = X5,
      'w-l%' = X6,
      'era' = X7,
      'g' = X8,
      'gs' = X9,
      'cg' = X10,
      'sho' = X11,
      'sv' = X12,
      'ip' = X13,
      'h' = X14,
      'r' = X15,
      'er' = X16,
      'hr' = X17,
      'bb' = X18,
      'so' = X19,
      'ir' = X20,
      'whip' = X21,
      'h/9' = X22,
      'hr/9' = X23,
      'bb/9' = X24,
      'so/9' = X25,
      'so/bb' = X26,
      'bk' = X27
    ) %>% 
    mutate(years = years) %>% 
    select(-X2) %>% 
    subset(edad != 'EDAD')
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching Df')
  pitching %>% 
    write_csv(path = paste0(years, "- pitching.csv"))
    
}
get_pitching(.URL)

#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)


#roster players ----
get_roster <- function(.URL){
  flog.info('Getting available URl')
  
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
    subset(jugador != 'Nombre')
  flog.info('Data Wrangling completed')
  
  flog.info('Roster Df')
  roster
}


# Batting players ---- 
get_batting <- function(.URL){
  flog.info('Getting available URl')
  
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
    )
  flog.info('Data Wrangling completed')
  
  
  flog.info('Batting Df')
  batting
}


# Pitching players ----
get_pitching <- function(.URL){
  flog.info('Getting available URl')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    as.data.frame() %>% 
    select(-X2) %>% 
    rename(
      'jugador' = X1,
      'edad' = X3,
      'w' = X4,
      'l' = X5,
      'w-l%' = X6,
      'era' = X7,
      'g' = X8,
      'gs' = X8,
      'cg' = X9,
      'sho' = X10,
      'sv' = X11,
      'ip' = X12,
      'h' = X13,
      'r' = X14,
      'er' = X15,
      'hr' = X16,
      'bb' = X17,
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
    subset(edad != 'EDAD') %>% 
    slice(1:(n()-3)
    )
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching Df')
  pitching
}

# complete_batting <- merge(roster, batting_, by.x = "jugador", by.y = "jugador") 

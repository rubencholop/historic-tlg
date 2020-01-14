#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)


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
  roster
}


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
  batting
}
# Batting stats Round Robin
get_batting_rr <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  batting_rr <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'g' = X3,
      'pa' = X4,
      'ab' = X4,
      'r' = X6,
      'h' = X7,
      '2b' = X8,
      '3b' = X9,
      'hr' = X10,
      'rbi' = X11,
      'sb' = X12,
      'cs' = X13,
      'bb' = X14,
      'so' = X15,
      'avg' = X16,
      'obp' = X17,
      'slg' = X18,
      'ops' = X19,
      'rc' = X20,
      'tb' = X21,
      'xb' = X22,
      'hbp' = X23,
      'sh' = X24,
      'sf' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(years = years) 
  # %>% 
  # select(years, jugador, edad, g, pa, ab, r, h, '2b', '3b', hr, rbi, sb, cs, bb,s,
  #        oavg, obp, slg, ops, ir, rc, tb, xb, hbp, sh, sf)
  flog.info('Data Wrangling completed')
  
  
  flog.info('Batting Df')
  batting_rr
}
# Batting stats final
get_batting_finals <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  batting_finals <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[4]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'g' = X3,
      'pa' = X4,
      'ab' = X4,
      'r' = X6,
      'h' = X7,
      '2b' = X8,
      '3b' = X9,
      'hr' = X10,
      'rbi' = X11,
      'sb' = X12,
      'cs' = X13,
      'bb' = X14,
      'so' = X15,
      'avg' = X16,
      'obp' = X17,
      'slg' = X18,
      'ops' = X19,
      'rc' = X20,
      'tb' = X21,
      'xb' = X22,
      'hbp' = X23,
      'sh' = X24,
      'sf' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  flog.info('Data Wrangling completed')
  
  
  flog.info('Batting Df')
  batting_finals
}
# Batting stats but with other order in the table
get_batting_finals1 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  batting_finals1 <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[3]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'g' = X3,
      'pa' = X4,
      'ab' = X4,
      'r' = X6,
      'h' = X7,
      '2b' = X8,
      '3b' = X9,
      'hr' = X10,
      'rbi' = X11,
      'sb' = X12,
      'cs' = X13,
      'bb' = X14,
      'so' = X15,
      'avg' = X16,
      'obp' = X17,
      'slg' = X18,
      'ops' = X19,
      'rc' = X20,
      'tb' = X21,
      'xb' = X22,
      'hbp' = X23,
      'sh' = X24,
      'sf' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  # %>% 
  # select(years, jugador, edad, g, pa, ab, r, h, '2b', '3b', hr, rbi, sb, cs, bb,s,
  #        oavg, obp, slg, ops, ir, rc, tb, xb, hbp, sh, sf)
  flog.info('Data Wrangling completed')
  
  
  flog.info('Batting Df')
  batting_finals1
}



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
  pitching
}
# Pitching players regular season 2
get_pitching2 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[5]] %>% 
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
  pitching
}
# Pitching players regular season 3 
get_pitching3 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[4]] %>% 
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
  pitching
}
# Pitching Round Robin stats 
get_pitching_rr <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[5]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    mutate(years = years) %>% 
    subset(edad != 'EDAD')
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching  RR ')
  pitching
}
# Pitching Round Robin stats 1
get_pitching_rr1 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  
  pitching <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[6]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    mutate(years = years) %>% 
    subset(edad != 'EDAD')
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching  RR ')
  pitching
}
# Pitching finals
get_pitching_finals1 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  pitching_finals <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[7]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching  finals ')
  pitching_finals
}
# Pitching finals but with other orden in the table
get_pitching_finals2 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  pitching_finals2 <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[5]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  flog.info('Data Wrangling completed')
  
  flog.info('Pitching  finals ')
  pitching_finals2
}
#Pitching finals 69-70
get_pitching_finals3 <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  pitching_finals2 <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[7]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  flog.info('Data Wrangling completed')
  pitching_finals2
}


# Fielding players ----
#fielding
get_fielding <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  campeon <- c('1964-65', '1965-66', '1968-69', '1970-71',
               '1982-1983', '1984-85', '1985-86')
  
  fielding <- read_html(.URL) %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[7]] %>% 
    as.data.frame() %>% 
    rename(
      'jugador' = X1,
      'edad' = X2,
      'w' = X3,
      'l' = X4,
      'w-l%' = X5,
      'era' = X6,
      'g' = X7,
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
      'so' = X18,
      'whip' = X19,
      'h/9' = X20,
      'hr/9' = X21,
      'bb/9' = X22,
      'so/9' = X23,
      'so/bb' = X24,
      'bk' = X25,
      'refuerzo' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    mutate(
      years = years,
      resultado = if_else(years %in% campeon, 'campeon', 'subcampeon')
    )
  flog.info('Data Wrangling completed')
  fielding
}





library(dplyr)
library(rvest)
library(futile.logger)

get_pitching <- function(.URL){
  flog.info('Getting available URl')
  years <- str_extract(.URL, '(?<=TIB&TE=).*')
  -
  pitching <- read_html('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1969-70') %>% 
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
      'so' = X18,
      'ir' = X19,
      'whip' = X20,
      'h/9' = X21,
      'hr/9' = X22,
      'bb/9' = X23,
      'so/9' = X24,
      'so/bb' = X25,
      'bk' = X26
    ) %>% 
    subset(edad != 'EDAD') %>% 
    slice(1:(n()-3)
    )
  
  #antes 3 en todos los años. Revisar igual
  1969-70 <- 5
  1970-71 <- 5
  1971-72 <- 5
  1972-73 <- 4
  1973-74 <- 3
  1974-75 <- 4
  1975-76 <- 0
  1976-77 <- 5
  1977-78 <- 4
  1978-79 <- 3
  1979-80 <- 4
  1980-81 <- 4
  1981-82 <- 4
  1982-83 <- 5
  1983-84 <- 4
  1984-85 <- 5
  1985-86 <- 5
  1986-87 <- 5
  1987-88 <- 3
  1988-89 <- 4
  1989-90 <- 4
  1991-92 <- 3
  1991-92 <- 4
  1992-93 <- 3
  
    
    
    
    
    
    
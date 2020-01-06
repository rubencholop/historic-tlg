#Libraries ----
library(dplyr)
library(rvest)



#Page year 1962-1963
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

#vector of years statistics baseball 
years <- c('1962-63')

#url with all years
.tgl_page <- paste('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=', years, sep = "" )

# Connecting to page
roster <- read_html(.tgl_page) %>% 
  html_nodes(css = '.sortable') %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  as.data.frame()


#Note: Falta eliminar la primera fila del df o header y eliminar primero columna de Roster y 
#ver como se eliminan las ultimas 3 o 4 filas de batting y pitching

batting_ <- read_html(.url) %>% 
  html_nodes(css = '.sortable') %>% 
  html_table(fill = TRUE) %>% 
  .[[2]] %>% 
  as.data.frame() 
   
pitching <- read_html(.url) %>% 
  html_nodes(css = '.sortable') %>% 
  html_table(fill = TRUE) %>% 
  .[[3]] %>% 
  as.data.frame()

data_batting <- batting_ %>% 
  subset(X3 != 'EDAD') %>% 
  select(-c('X2')) %>% 
  rename(
    'jugador' = X1,
    'edad' = X3,
    'G' = X4,
    'ab' = X5,
    'r' = X6,
    'h' = X7,
    '2b' = X8,
    '3b' = X9,
    'hr' = X10,
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
  )  

  



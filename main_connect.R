#Libraries

library(dplyr)
library(rvest)
library()
g

#Page
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

# Connecting to page
webpage <- read_html(.url) %>% 
  html_nodes(css = '.sortable') %>% 
  html_text() %>% 
  as.data.frame()

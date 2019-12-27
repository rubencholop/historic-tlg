#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
#page http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63


# Connect to Pelota binaria ----
#Page year 1962-1963
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

#vector of years statistics baseball 
years <- c('1962-63')

#url with all years
.tgl_page <- paste('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=', years, sep = "" )



-------------------------------------------------------
  
from <- 1962
to <- lubridate::year(Sys.Date())
range_ <- c(.desde:.hasta)
pages <- c(1:(from - to +1))


take_years <-  function(x){
  df <- paste(
    "http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=", range_[x], "-", substring(range_[x+1],3),
    sep=""
  )
  data.frame(df)
}

# Getting the data
URL <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
)
URL <- as.character(URL$b[.pages])


Catcher1 <- data.frame(Equipo = character(), P_URL = character())
for(i in URL) {
  df1 <- read_html(i)
  Equipo <- df1 %>% 
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] 
  
  P_URL <- df1 %>%
    html_nodes(css = '.sortable') %>% 
    html_table(fill = TRUE) %>% 
    .[[2]] 
  
  temp <- data.frame(Equipo, P_URL)
  Catcher1 <- rbind(Catcher1, temp)
  cat("*")
}

fin <- length(Catcher1$Equipo)

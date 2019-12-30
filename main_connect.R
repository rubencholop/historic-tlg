#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
library(progress)
#page http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63


# Connect to Pelota binaria ----
#Page year 1962-1963
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

#vector of years statistics baseball 
years <- c('1962-63')

#url with all years
.tgl_page <- paste('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=', years, sep = "" )



#-------------------------------------------------------
  
from <- 1962
to <- lubridate::year(Sys.Date()) + 1
range_ <- c(from:to)
pages <- c(1:(to - (from )))


take_years <-  function(x){
  df <- paste(
    "http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=", range_[x], "-", substring(range_[x+1],3),
    sep=""
  )
  data.frame(df)
}

# Getting the data
URLs <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
)
URLs <- as.character(URLs$df[pages])
all_rosters <- purrr::map(URLs, get_roster) 
historic_roster <- data.table::rbindlist(data_frames,
                                              fill = TRUE)


.test <- all_rosters[-c(14)]

# .pb <- progress_bar$new(total = 100)
# for (i in 1:100) {
#   .pb$tick()
#   Sys.sleep(1 / 100)
# }


# URLs <- as.character(URL$b[.pages])

# for(i in URLs) {
#   df1 <- read_html(i)
#   Equipo <- df1 %>% 
#     html_nodes(css = '.sortable') %>% 
#     html_table(fill = TRUE) %>% 
#     .[[1]] 
#   
#   P_URL <- df1 %>%
#     html_nodes(css = '.sortable') %>% 
#     html_table(fill = TRUE) %>% 
#     .[[2]] 
#   
#   temp <- data.frame(Equipo, P_URL)
#   Catcher1 <- rbind(Catcher1, temp)
#   cat("*")
# }

#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
library(progress)
library(stringr)
library(purrr)
#page http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63


# Connect to Pelota binaria ----
#P age year 1962-1963
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

# Vector of years statistics baseball 
years <- c('1962-63')

# Url with all years
.tgl_page <- paste('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=', years, sep = "" )

# Function to get a list with df ----
from <- 1962
to <- lubridate::year(Sys.Date()) 
range_ <- c(from:to)
pages <- c(1:(to - (from )))

take_years <-  function(x){
  df <- paste(
    "http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=", range_[x], "-", substring(range_[x+1],3),
    sep=""
  )
  data.frame(df)
}

# Getting Roster hictoric data ----
URLs <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs <- as.character(URLs$df[pages])
all_rosters <- map(URLs, get_roster) %>% 
  keep(function(x) nrow(x) > 0)

#Historic roster df
historic_roster <- data.table::rbindlist(all_rosters,
                                              fill = TRUE)

# Getting batting  historic data ----
URLs_batting <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_batting <- as.character(URLs_batting$df[pages])
all_batting_df <- map(URLs, get_batting) 

# Historic batting df
historic_batting_df <- data.table::rbindlist(all_batting_df,
                                         fill = TRUE)

# Getting pitching  historic data ----
URLs_pitching <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_pitching <- as.character(URLs_pitching$df[pages])
all_pitching_df <- map(URLs_pitching, get_pitching) 

# Historic batting df
historic_pitching_df <- data.table::rbindlist(all_pitching_df,
                                             fill = TRUE)







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
>>>>>>> 83c027cf96946297f8e3fe6583bdd7b9e3ec1365

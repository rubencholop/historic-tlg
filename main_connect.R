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
                                              fill = TRUE) %>% 
  select(years, jugador, name, pos, bat, lan, exp, pais, estado, ciudad)
  

# Getting batting  historic data ----
URLs_batting_tem_reg <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_batting_tem_reg <- as.character(URLs_batting_tem_reg$df[pages])
all_batting_tem_reg <- map(URLs_batting_tem_reg, get_batting) 

# Historic batting df in regular season
historic_batting_tem_reg <- data.table::rbindlist(all_batting_tem_reg,
                                         fill = TRUE) %>% 
  select(years, jugador, edad, g, pa, ab, r, h, '2b', '3b', hr, rbi, 
         sb, cs, bb, so, avg, obp, slg, ops, ir, rc, tb, xb, hbp,
         sh, sf)

# Historic batting df in Round robin ----
URLs_batting_rr <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_batting_rr <- as.character(URLs_batting_rr$df[pages])
years_rr <- c(8:11, 13, 15:16, 18:25, 27:28, 30, 39, 42, 46:48, 50, 52:55, 58)
URLs_rr <- URLs_batting_rr[years_rr]
all_batting_rr <- map(URLs_rr, get_batting_rr) 
historic_batting_rr <- data.table::rbindlist(all_batting_rr,
                                              fill = TRUE)

# Historic batting df in Finals ----
URLs_batting_finals <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_batting_finals <- as.character(URLs_batting_finals$df[pages])
years_finals <- c(5, 8:10, 13, 15, 21, 23:25, 50)
URLs_finals <- URLs_batting_finals[years_finals]
all_batting_finals <- map(URLs_finals, get_batting_finals) 
historic_batting_finals <- data.table::rbindlist(all_batting_finals,
                                             fill = TRUE) %>% 
  select(1:28)

# Others finals but with distinct order in the table
URLs_batting_finals1 <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_batting_finals1 <- as.character(URLs_batting_finals1$df[pages])
years_finals1 <- c(3:4, 7)
URLs_finals1 <- URLs_batting_finals1[years_finals1]
all_batting_finals1 <- map(URLs_finals1, get_batting_finals1) 
historic_batting_finals1 <- data.table::rbindlist(all_batting_finals1,
                                                 fill = TRUE)

# Battinf df global in finals
batting_finals <- rbind(historic_batting_finals, historic_batting_finals1)
write.csv(batting_finals, file = 'data/batting_finals.csv')



# Getting pitching  historic data ----
URLs_pitching <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_pitching <- as.character(URLs_pitching$df[pages])
all_pitching_df <- map(URLs_pitching, get_pitching) 

# Historic pitching df
historic_pitching_df <- data.table::rbindlist(all_pitching_df,
                                             fill = TRUE)


# Getting pitching Round Robin ----
URLs_pitching_rr <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_pitching <- as.character(URLs_pitching$df[pages])
years_rr <- c(8:11, 13, 15:16, 18:25, 27:28, 30, 39, 42, 46:48, 50, 52:55, 58)
URLs_rr <- URLs_pitching_rr[years_rr]
all_pitching_rr <- map(URLs_pitching_rr, get_pitching_rr) 

# Historic pitching df
historic_pitching_df <- data.table::rbindlist(all_pitching_df,
                                              fill = TRUE)

# Getting pitching Finals ----
URLs_pitching_finals <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 
URLs_pitching_finals <- as.character(URLs_pitching_finals$df[pages])
years_finals <- c(5, 8:10, 13, 15, 21, 23:25, 50)
URLs_finals <- URLs_pitching_finals1[years_finals]
all_pitching_finals <- map(URLs_pitching_finals, get_pitching_finals) 

# Historic pitching df
historic_pitching_finals <- data.table::rbindlist(all_pitching_finals,
                                              fill = TRUE)






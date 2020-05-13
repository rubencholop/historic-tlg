# Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)
library(data.table)
library(progress)
library(stringr)
library(purrr)
library(tidyselect)
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
pages <- c(1:(to - (from)))

take_years <-  function(x){
  df <- paste(
    "http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=", range_[x], "-", substring(range_[x+1],3),
    sep=""
  )
  data.frame(df)
}


season <-  function(x){
  df <- paste(range_[x], "-", substring(range_[x + 1], 3), sep="")
  data.frame(df)
}

seasons <- rbindlist(
  lapply(pages, season), fill = TRUE
) %>% 
  arrange(desc(df)) %>% 
  rename(temporadas = df) %>% 
  pull()
  

# Getting Roster hictoric ----
URLs <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 

URLs <- as.character(URLs$df[pages])
all_rosters <- map(URLs, get_roster) %>% 
  keep(function(x) nrow(x) > 0)

#Historic roster df
Rosters <- data.table::rbindlist(all_rosters,
                                              fill = TRUE) %>% 
  select(years, jugador, name, pos, bat, lan, exp, pais, estado, ciudad)
write.csv(Rosters, file = 'data/rosters.csv')
  

# Getting batting regular season ----
batting_reseason <- map(URLs, get_batting) 
Hbatting_reseason <- data.table::rbindlist(batting_reseason,
                                         fill = TRUE) %>% 
  select(years, jugador, edad, g, pa, ab, r, h, '2b', '3b', hr, rbi, 
         sb, cs, bb, so, avg, obp, slg, ops, ir, rc, tb, xb, hbp,
         sh, sf)
write.csv(Hbatting_reseason, file = 'data/batting_reseason.csv')

# Historic batting df in Round robin ----
years_rr <- c(4:5, 8:11, 13, 15:16, 18:25, 27:28, 30, 39, 42, 46:48, 50, 52:55)
URLs_rr <- URLs[years_rr]
batting_rr <- map(URLs_rr, get_batting_rr) 
Hbatting_rr <- data.table::rbindlist(batting_rr,
                                              fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Jugadores|Refuerzos|Titulares'))
         )

write.csv(Hbatting_rr, file = 'data/batting_rr.csv')

# Historic batting df in Finals ----
years_finals <- c(5, 8:10, 15, 21, 23:25, 50)
URLs_finals <- URLs[years_finals]
batting_finals <- map(URLs_finals, get_batting_finals) 
batting_finals1 <- data.table::rbindlist(batting_finals,
                                             fill = TRUE) %>% 
  select(years, 1:28) %>% 
  filter(!(str_detect(jugador, 'Jugadores|Refuerzos|Titulares'))
  )

# Others finals but with distinct order in the table
years_finals1 <- c(3:4, 7)
URLs_finals1 <- URLs[years_finals1]
batting_finals12 <- map(URLs_finals1, get_batting_finals1) 
batting_finals2 <- data.table::rbindlist(batting_finals12,
                                                 fill = TRUE) %>% 
  select(years, 1:28) %>% 
  filter(!(str_detect(jugador, 'Jugadores|Refuerzos|Titulares'))
  )
  
# Battinf  finals
Hbatting_finals <- rbind(batting_finals1, batting_finals2)
write.csv(Hbatting_finals, file = 'data/batting_finals.csv')


# Getting pitching  regular season ----
# pitching df1
years_tem_reg <- c(1:2, 12, 17, 26, 29, 31:38, 40:41, 43:45, 49, 51, 56:57)
URLs_temp_reg <- URLs[years_tem_reg]
pitching_tem_reg1 <- map(URLs_temp_reg, get_pitching) 
pitching_reseason1 <- data.table::rbindlist(pitching_tem_reg1,
                                             fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Peloteros|Importados|Criollos'))
  )

# pitching df2
years_tem_reg2 <- c(4:5, 8:10, 15, 21, 23:25, 50)
URLs_temp_reg2 <- URLs[years_tem_reg2]
pitching_tem_reg2 <- map(URLs_temp_reg2, get_pitching2)  
pitching_reseason2 <- data.table::rbindlist(pitching_tem_reg2,
                                                   fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Peloteros|Importados|Criollos'))
  )

# pitching df3
years_tem_reg3 <- c(3, 6:7, 11, 13, 16, 18:20, 22, 27:28, 30, 39, 42,
                    46:48, 52:55)
URLs_temp_reg3 <- URLs[years_tem_reg3]
pitching_tem_reg3 <- map(URLs_temp_reg3, get_pitching3)  
pitching_reseason3 <- data.table::rbindlist(pitching_tem_reg3,
                                               fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Peloteros|Importados|Criollos'))
  )

# Pitching regolar season df
pitching_rs <- rbind(pitching_reseason1, pitching_reseason2, pitching_reseason3) %>% 
  arrange(years)
# write csv file 
write.csv(pitching_rs, file = 'data/pitching_reseason.csv')


# Getting pitching Round Robin ----
#Pitching rr df1
years_rr <- c(11, 13, 16, 18:20, 22, 27:28, 30, 39, 42, 46:48, 52:55)
URLs_pit_rr <- URLs[years_rr]
pitching_rr <- map(URLs_pit_rr, get_pitching_rr) 
pitching_rrobin1 <- data.table::rbindlist(pitching_rr,
                                              fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Refuerzos|Titulares|Peloteros'))
  )
  
#Pitching rr df2
years_rr1 <- c(4:5, 8:10, 15, 21, 23:25, 50)
URLs_pit_rr1 <- URLs[years_rr1]
pitching_rr1 <- map(URLs_pit_rr1, get_pitching_rr1) 
pitching_rrobin2 <- data.table::rbindlist(pitching_rr1,
                                              fill = TRUE) %>% 
  select(years, 1:26) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Refuerzos|Titulares|Peloteros'))
  )

Pitching_rrobin <- rbind(pitching_rrobin1, pitching_rrobin2)

write.csv(Pitching_rrobin, file = 'data/pitching_rrobin.csv')


# Getting pitching Finals ----
# Historic pitching finals df1
years_finals1 <- c(4:5, 9:10, 15, 21, 23:25, 50)
URLs_finals1 <- URLs[years_finals1]
pitching_fin1 <- map(URLs_finals1, get_pitching_finals1) 
pitching_finals1 <- data.table::rbindlist(pitching_fin1,
                                              fill = TRUE) %>% 
  select(years, 1:28) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Refuerzos|Titulares|Peloteros'))
  )

# Historic pitching finals df2
years_finals2 <- c(3, 7)
URLs_finals2 <- URLs[years_finals2]
pitching_fin2 <- map(URLs_finals2, get_pitching_finals2) 
pitching_finals2 <- data.table::rbindlist(pitching_fin2,
                                                  fill = TRUE) %>% 
  select(years, 1:28) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Refuerzos|Titulares|Peloteros'))
  )

# Historic pitching finals df3
years_finals3 <- c(8)
URLs_finals3 <- URLs[years_finals3]
pitching_fin3 <- map(URLs_finals3, get_pitching_finals3) 
pitching_finals3 <- data.table::rbindlist(pitching_fin3,
                                                   fill = TRUE) %>% 
  select(years, 1:28) %>% 
  arrange(jugador) %>% 
  filter(!(str_detect(jugador, 'Refuerzos|Titulares|Peloteros'))
  )


# Historic pitching finals
pitching_finals <- rbind(pitching_finals1,
                         pitching_finals2,
                         pitching_finals3) %>% 
  arrange(years)

write.csv(pitching_finals, file = 'data/pitching_finals.csv')



# Falta fielding tables ----


# Data sets ----
Rosters$jugador[[2487]] <-  'A. Álvares' # To different names with others names in df
Rosters$jugador[[1323]] <-  'L. Salazar' # To different names with others names in df

# Distinct jugadores ----
distinct_players <- Rosters %>% 
  distinct(jugador) %>% 
  arrange(jugador) 

# Distinct  temporadas ---- 
  distinct_years <- Rosters %>% 
  select(years) %>% 
  distinct(years) %>% 
  arrange(desc(years)) %>% 
  pull()

  # choices_years <- as.numeric(distinct_years)
  
# Distinct bats ----
  distinct_bats <- Rosters %>% 
    filter(pos != 'P') %>% 
    select(jugador, name, pos) %>% 
    arrange(jugador) %>% 
    distinct(jugador)
# Distinct lanzadores ----
  distinct_lan <- Rosters %>% 
    filter(pos == 'P') %>% 
    select(jugador) %>% 
    arrange(jugador) %>% 
    distinct(jugador)

# Tablas de informacion ----
Hbf <- Hbatting_finals %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28) %>% 
  left_join(distinct_players, by = c('key')) %>% 
  select(key, years, jugador, name, 4:30) 


Hbrs <- Hbatting_reseason %>% 
  mutate(key = paste(as.character(years), jugador))

Hbrr  <- Hbatting_rr %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hprs <- pitching_rs %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hprr <- Pitching_rrobin %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:27)

Hpf <- pitching_finals %>% 
  mutate(key = paste(as.character(years), jugador)) %>%
  select(key, 1:29)

list_years <- c("2019-20",
                "2018-19",
                "2017-18",
                "2016-17", 
                "2015-16",
                "2014-15",
                "2013-14",
                "2012-13",
                "2011-12",
                "2010-11",
                "2009-10"
                )


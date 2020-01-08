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
historic_roster <- data.table::rbindlist(all_rosters,
                                              fill = TRUE) %>% 
  select(years, jugador, name, pos, bat, lan, exp, pais, estado, ciudad)
  

# Getting batting regular season ----
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



# Getting pitching  regular season ----
URLs_pitching <- rbindlist(
  lapply(
    pages, take_years),
  fill = TRUE
) 

# pitching df1
URLs_pitching_tem_reg <- as.character(URLs_pitching$df[pages])
years_tem_reg <- c(1:2, 12, 17, 26, 29, 31:38, 40:41, 43:45, 49, 51, 56:57)
URLs_temp_reg <- URLs_pitching_tem_reg[years_tem_reg]
pitching_tem_reg1 <- map(URLs_temp_reg, get_pitching) 
all_pitching_tem_reg1 <- data.table::rbindlist(pitching_tem_reg1,
                                             fill = TRUE) %>% 
  select(years, 1:26)

# pitching df2
URLs_pitching_tem_reg2 <- as.character(URLs_pitching$df[pages])
years_tem_reg2 <- c(4:5, 8:10, 15, 21, 23:25, 50)
URLs_temp_reg2 <- URLs_pitching_tem_reg[years_tem_reg2]
pitching_tem_reg2 <- map(URLs_temp_reg2, get_pitching2)  
all_pitching_tem_reg2 <- data.table::rbindlist(pitching_tem_reg2,
                                                   fill = TRUE) %>% 
  select(years, 1:26)

# pitching df3
URLs_pitching_tem_reg3 <- as.character(URLs_pitching$df[pages])
years_tem_reg3 <- c(3, 6:7, 11, 13, 16, 18:20, 22, 27:28, 30, 39, 42,
                    46:48, 52:55)
URLs_temp_reg3 <- URLs_pitching_tem_reg[years_tem_reg3]
pitching_tem_reg3 <- map(URLs_temp_reg3, get_pitching3)  
all_pitching_tem_reg3 <- data.table::rbindlist(pitching_tem_reg3,
                                               fill = TRUE) %>% 
  select(years, 1:26)

# Pitching regolar season df
pitching_rs <- rbind(all_pitching_tem_reg1, all_pitching_tem_reg3, all_pitching_tem_reg2) %>% 
  arrange(years)
# write csv file 
write.csv(pitching_rs, file = 'data/pitching_rs.csv')


# Getting pitching Round Robin ----
URLs_pitching_rr <- as.character(URLs_pitching$df[pages])
years_rr <- c(11, 13, 16, 18:20, 22, 27:28, 30, 39, 42, 46:48, 52:55)
URLs_pit_rr <- URLs_pitching_rr[years_rr]
all_pitching_rr <- map(URLs_pit_rr, get_pitching_rr) 
historic_pitching_rr <- data.table::rbindlist(all_pitching_rr,
                                              fill = TRUE) %>% 
  select(years, 2:26)
  
write.csv(historic_pitching_rr, file = 'data/pitching_rr.csv')


# Getting pitching Finals ----
# Historic pitching finals df1
URLs_pitching_finals1 <- as.character(URLs_pitching$df[pages])
years_finals1 <- c(4:5, 9:10, 15, 21, 23:25, 50)
URLs_finals1 <- URLs_pitching_finals1[years_finals1]
all_pitching_finals1 <- map(URLs_finals1, get_pitching_finals1) 
historic_pitching_finals <- data.table::rbindlist(all_pitching_finals1,
                                              fill = TRUE) %>% 
  select(years, 1:28)

# Historic pitching finals df2
URLs_pitching_finals2 <- as.character(URLs_pitching$df[pages])
years_finals2 <- c(3, 7)
URLs_finals2 <- URLs_pitching_finals2[years_finals2]
all_pitching_finals2 <- map(URLs_finals2, get_pitching_finals2) 
historic_pitching_finals2 <- data.table::rbindlist(all_pitching_finals2,
                                                  fill = TRUE) %>% 
  select(years, 1:28)

# Historic pitching finals df3
URLs_pitching_finals3 <- as.character(URLs_pitching$df[pages])
years_finals3 <- c(8)
URLs_finals3 <- URLs_pitching_finals3[years_finals3]
all_pitching_finals3 <- map(URLs_finals3, get_pitching_finals3) 
historic_pitching_finals3 <- data.table::rbindlist(all_pitching_finals3,
                                                   fill = TRUE) %>% 
  select(years, 1:28)

#Historic pitching finals
pitching_finals <- rbind(historic_pitching_finals,
                         historic_pitching_finals2,
                         historic_pitching_finals3) %>% 
  arrange(years)
write.csv(pitching_finals, file = 'data/pitching_finals.csv')



#Nota:
#Revisar que tengan el numero de columnas igual que en realidad Pitching rr y todos los
#demas df menos pitching finals porque ya fue auditado. revisar la 69-70 tiene 3 filas al
#final y no 4

# Hacer una funcion completa para toda esta carga de datos ya que no es un cogido bien
# leible
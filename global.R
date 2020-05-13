# Libraries ---
library(readr)
library(dplyr)


# Data ----
Rosters <- read_csv('data/rosters.csv')
brs <- read_csv('data/batting_reseason.csv')
brr <- read_csv('data/batting_rr.csv')
bf <- read_csv('data/batting_finals.csv')
prs <- read_csv('data/pitching_reseason.csv')
prr <- read_csv('data/pitching_rrobin.csv')
pf <- read_csv('data/pitching_finals.csv')

.rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  distinct(name) %>% 
  mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
  )

Rosters <- Rosters %>% 
  arrange(jugador, years) %>% 
  left_join(.rosters, by = 'name')

Unique_Rosters <- Rosters %>% 
  group_by(ID) %>% 
  summarize(
    jugador = last(jugador),
    name = last(name),
    pos = last(pos),
    bat = last(bat),
    lan = last(lan),
    exp = last(exp),
    pais = last(pais),
    estado = last(estado),
    ciudad = last(ciudad)
  ) %>% 
  arrange(jugador)

# Data wrangling ----
.pitchers <- Rosters %>% 
  filter(pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarise(jugador = last(jugador)) %>% 
  arrange(jugador) %>% 
  select(jugador)

.bateadores <- Rosters %>% 
  filter(!pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarise(jugador = last(jugador)) %>% 
  arrange(jugador) %>% 
  select(jugador)


  
  
.pitchers$jugador[[177]] <-  'C. Hernandez' # To different names with others names in df
.pitchers$jugador[[347]] <-  'C. Hernandez' # To different names with others names in df


# Distinct jugadores ----
distinct_players <- Rosters %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, name)
  # distinct(jugador) %>% 
  # arrange(jugador) 

# Distinct  temporadas ---- 
distinct_years <- Rosters %>% 
  select(years) %>% 
  distinct(years) %>% 
  arrange(desc(years)) %>% 
  pull()

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

# Df all info ----
Hbf <- bf %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28) %>% 
  left_join(distinct_players, by = c('key')) %>%
  select(key, years, jugador, name, 4:30) 


Hbrs <- brs %>% 
  mutate(key = paste(as.character(years), jugador))

Hbrr  <- brr %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hprs <- prs %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hprr <- prr %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:27)

Hpf <- pf %>% 
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

# Df by seasons ----

Pby_season <- Hprs

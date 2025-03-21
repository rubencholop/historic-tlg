# Libraries ----
library(readr)
library(dplyr)
library(xml2)
library(readxl)
library(rvest)


# Data ----
Palmares <- read_csv('data/Palmares.csv')
Numeros_retirado <- read_csv('data/numeros_retirados.csv')
Records <- read_csv('data/records.csv')
brs <- read_csv('data/batting_reseason.csv')
brr <- read_csv('data/batting_rr.csv')
bf <- read_csv('data/batting_finals.csv')
prs <- read_csv('data/pitching_reseason.csv')
prr <- read_csv('data/pitching_rrobin.csv')
pf <- read_csv('data/pitching_finals.csv')
Roster_clean <- read_csv('data/rosters_clean.csv')

.rosters <- Roster_clean %>% 
  arrange(jugador) %>% 
  distinct(name) %>% 
  mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
  )

Roster_clean <- Roster_clean %>% 
  arrange(jugador, years) %>% 
  left_join(.rosters, by = 'name') %>% 
  group_by(ID) %>% 
  summarize(
    jugador = last(jugador),
    name = last(name),
    first_name = last(first_name),
    last_name = last(last_name),
    pos = last(pos),
    bat = last(bat),
    lan = last(lan),
    exp = last(exp),
    pais = last(pais),
    estado = last(estado),
    ciudad = last(ciudad),
    .groups = "drop"
  ) %>% 
  arrange(jugador)


  

# Data wrangling ----
.pitchers <- Rosters %>% 
  filter(pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = last(jugador)) %>% 
  arrange(jugador) %>% 
  select(jugador)

.bateadores <- Rosters %>% 
  filter(!pos == 'P') %>% 
  arrange(jugador, years) %>% 
  group_by(ID) %>% 
  summarize(jugador = last(jugador)) %>% 
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

Hbrs <- brs %>% 
  mutate(key = paste(as.character(years), jugador))

Hbrr  <- brr %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hbf <- bf %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28) %>% 
  left_join(distinct_players, by = c('key')) %>%
  select(key, years, jugador, name, 4:30) 

Hprs <- prs %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:28)

Hprr <- prr %>% 
  mutate(key = paste(as.character(years), jugador)) %>% 
  select(key, 1:27)

Hpf <- pf %>% 
  mutate(key = paste(as.character(years), jugador)) %>%
  select(key, 1:29)

# List of years ----  
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
                "2009-10",
                "2008-09",
                "2007-08",
                "2006-07",
                "2005-06"
)

# Df by seasons by pitching ----
Pby_season <- prs %>% 
  arrange(years, jugador) %>% 
  select(-bk) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad, na.rm = T), 1),
    w = sum(w, na.rm = T),
    l = sum(l, na.rm = T),
    era = round(mean(era, na.rm = T), 2),
    g = sum(g, na.rm = T),
    gs = sum(gs, na.rm = T),
    gp = w + l,
    cg = sum(cg, na.rm = T),
    sho = sum(sho, na.rm = T),
    sv = sum(sv, na.rm = T),
    ip = sum(ip, na.rm = T),
    h = sum(h, na.rm = T),
    r = sum(r, na.rm = T),
    er = sum(er, na.rm = T),
    hr = sum(hr, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    ir = sum(ir, na.rm = T),
    whip = round(mean(whip, na.rm = T), 2),
    `h/9` = round(mean(`h/9`, na.rm = T), 2),
    `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
    `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
    `so/9` = round(mean(`so/9`, na.rm = T), 2),
    `so/bb` = round(mean(`so/bb`, na.rm = T), 2)
  )


Pby_rr <- prr %>% 
  arrange(years, jugador) %>% 
  select(-bk) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad), 1),
    w = sum(w, na.rm = T),
    l = sum(l, na.rm = T),
    era = round(mean(era, na.rm = T), 2),
    g = sum(g, na.rm = T),
    gs = sum(gs, na.rm = T),
    gp = w + l,
    cg = sum(cg, na.rm = T),
    sho = sum(sho, na.rm = T),
    sv = sum(sv, na.rm = T),
    ip = sum(ip, na.rm = T),
    h = sum(h, na.rm = T),
    r = sum(r, na.rm = T),
    er = sum(er, na.rm = T),
    hr = sum(hr, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    whip = round(mean(whip, na.rm = T), 2),
    `h/9` = round(mean(`h/9`, na.rm = T), 2),
    `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
    `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
    `so/9` = round(mean(`so/9`, na.rm = T), 2),
    `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
    refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
  )


Pby_final <-  pf %>% 
  arrange(years, jugador) %>% 
  select(-bk) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad), 1),
    w = sum(w, na.rm = T),
    l = sum(l, na.rm = T),
    era = round(mean(era, na.rm = T), 2),
    g = sum(g, na.rm = T),
    gs = sum(gs, na.rm = T),
    gp = w + l,
    cg = sum(cg, na.rm = T),
    sho = sum(sho, na.rm = T),
    sv = sum(sv, na.rm = T),
    ip = round(sum(ip, na.rm = T), 1),
    h = sum(h, na.rm = T),
    r = sum(r, na.rm = T),
    er = sum(er, na.rm = T),
    hr = sum(hr, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    whip = round(mean(whip, na.rm = T), 2),
    `h/9` = round(mean(`h/9`, na.rm = T), 2),
    `hr/9` = round(mean(`hr/9`, na.rm = T), 2),
    `bb/9` = round(mean(`bb/9`, na.rm = T), 2),
    `so/9` = round(mean(`so/9`, na.rm = T), 2),
    `so/bb` = round(mean(`so/bb`, na.rm = T), 2),
    refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
    resultado = last(resultado)
  )


# Df by seasons by batting ----

Bby_season <- brs %>% 
  arrange(years, jugador) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad), 1),
    g = sum(g, na.rm = T),
    pa = sum(pa, na.rm = T),
    ab = sum(ab, na.rm = T),
    r = sum(r, na.rm = T),
    h = sum(h, na.rm = T),
    `2b` = sum(`2b`, na.rm = T),
    `3b` = sum(`3b`, na.rm = T),
    hr = sum(hr, na.rm = T),
    rbi = sum(rbi, na.rm = T),
    sb = sum(sb, na.rm = T),
    cs = sum(cs, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    avg = round(mean(avg, na.rm = T), 3),
    obp = round(mean(obp, na.rm = T), 3),
    slg = round(mean(slg, na.rm = T), 3),
    ops = round(mean(ops, na.rm = T), 3),
    ir = sum(ir, na.rm = T),
    rc = sum(rc, na.rm = T),
    tb = sum(tb, na.rm = T),
    xb = sum(xb, na.rm = T),
    hbp = sum(hbp, na.rm = T),
    sh = sum(sh, na.rm = T),
    sf = sum(sf, na.rm = T)
  )


Bby_rr <- brr %>% 
  arrange(years, jugador) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad), 1),
    g = sum(g, na.rm = T),
    X5 = sum(X5, na.rm = T),
    ab = sum(ab, na.rm = T),
    r = sum(r, na.rm = T),
    h = sum(h, na.rm = T),
    `2b` = sum(`2b`, na.rm = T),
    `3b` = sum(`3b`, na.rm = T),
    hr = sum(hr, na.rm = T),
    rbi = sum(rbi, na.rm = T),
    sb = sum(sb, na.rm = T),
    cs = sum(cs, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    avg = round(mean(avg, na.rm = T), 3),
    obp = round(mean(obp, na.rm = T), 3),
    slg = round(mean(slg, na.rm = T), 3),
    ops = round(mean(ops, na.rm = T), 3),
    rc = sum(rc, na.rm = T),
    tb = sum(tb, na.rm = T),
    xb = sum(xb, na.rm = T),
    hbp = sum(hbp, na.rm = T),
    sh = sum(sh, na.rm = T),
    sf = sum(sf, na.rm = T),
    refuerzo = sum(ifelse(refuerzo =='SI', 1, 0))
  )


Bby_final <- bf %>% 
  arrange(years, jugador) %>% 
  group_by(years) %>% 
  summarise(
    edad = round(mean(edad), 1),
    g = sum(g, na.rm = T),
    X5 = sum(X5, na.rm = T),
    ab = sum(ab, na.rm = T),
    r = sum(r, na.rm = T),
    h = sum(h, na.rm = T),
    `2b` = sum(`2b`, na.rm = T),
    `3b` = sum(`3b`, na.rm = T),
    hr = sum(hr, na.rm = T),
    rbi = sum(rbi, na.rm = T),
    sb = sum(sb, na.rm = T),
    cs = sum(cs, na.rm = T),
    bb = sum(bb, na.rm = T),
    so = sum(so, na.rm = T),
    avg = round(mean(avg, na.rm = T), 3),
    obp = round(mean(obp, na.rm = T), 3),
    slg = round(mean(slg, na.rm = T), 3),
    ops = round(mean(ops, na.rm = T), 3),
    rc = sum(rc, na.rm = T),
    tb = sum(tb, na.rm = T),
    xb = sum(xb, na.rm = T),
    hbp = sum(hbp, na.rm = T),
    sh = sum(sh, na.rm = T),
    sf = sum(sf, na.rm = T),
    refuerzo = sum(ifelse(refuerzo =='SI', 1, 0)),
    resultado = last(resultado)
  ) 

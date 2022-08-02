# Libraries ----
library(dplyr)
library(readr)
source("analysis/Dataset.R")

# Auxiliar functions ----
IP <- function(x){
  x <- x %>%
    as.character()
  
  episodio <-  as.numeric(sub("\\..*", "", x))
  episodio <- sum(episodio)
  
  tercio <- dplyr::case_when(stringr::str_detect(x,"\\.") ~ stringr::str_sub(x, -1, -1),
                             TRUE ~ "0")
  tercio <- as.numeric(tercio)
  tercio <- sum(tercio)
  
  x <-  episodio + trunc(tercio / 3) + ((tercio %% 3) / 10)
  
  return(x)
}
# Analysis by decade pitching (regular season) ----

decades_name <- as.factor(c("60","70", "80", "90", "00", "10", "20"))


pit_decade <- prs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(decade) %>%
  summarise(
    seasons = n_distinct(years),
    w = sum(w, na.rm = TRUE),
    l = sum(l, na.rm = TRUE),
    # g = sum(g, na.rm = TRUE),
    gs = sum(gs, na.rm = TRUE),
    cg = sum(cg, na.rm = TRUE),
    sho = sum(sho, na.rm = TRUE),
    sv = sum(sv, na.rm = TRUE),
    ip = sum(IP(ip), na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    er = sum(er, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    # era = (ip / er) * 9,
    era = round((er / IP(ip)) * 9, 3),
    whip = round((bb + h) / IP(ip), 3),
    `h/9` = round((h / IP(ip)) * 9, 2),
    `hr/9` = round((hr / IP(ip)) * 9, 2),
    `bb/9` = round((bb / IP(ip)) * 9, 2),
    `so/9` = round((so / IP(ip)) * 9, 2),
    `so/bb` = round(so / bb, 2),
    .groups = "drop"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    # decade = as.factor(decade),
    decade = forcats::fct_relevel(
      decade, "60","70", "80", "90", "00", "10", "20"
      )
    ) %>% 
  arrange(decade)


# group_by(years, nacionalidad) %>%
# dplyr::filter(nacionalidad == "Importad") %>% 
top_n(20, years)
  # 
# Analysis by decade - nacionalidad pitching (regular season) ----

decades_name <- as.factor(c("60","70", "80", "90", "00", "10", "20"))


pit_decade_nat <- prs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(decade, nacionalidad) %>%
  summarise(
    seasons = n_distinct(years),
    w = sum(w, na.rm = TRUE),
    l = sum(l, na.rm = TRUE),
    # g = sum(g, na.rm = TRUE),
    gs = sum(gs, na.rm = TRUE),
    cg = sum(cg, na.rm = TRUE),
    sho = sum(sho, na.rm = TRUE),
    sv = sum(sv, na.rm = TRUE),
    ip = sum(IP(ip), na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    er = sum(er, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    # era = (ip / er) * 9,
    era = round((er / IP(ip)) * 9, 3),
    whip = round((bb + h) / IP(ip), 3),
    `h/9` = round((h / IP(ip)) * 9, 2),
    `hr/9` = round((hr / IP(ip)) * 9, 2),
    `bb/9` = round((bb / IP(ip)) * 9, 2),
    `so/9` = round((so / IP(ip)) * 9, 2),
    `so/bb` = round(so / bb, 2),
    .groups = "drop"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    # decade = as.factor(decade),
    decade = forcats::fct_relevel(
      decade, "60","70", "80", "90", "00", "10", "20"
      )
    ) %>% 
  arrange(decade)


# group_by(years, nacionalidad) %>%
# dplyr::filter(nacionalidad == "Importad") %>% 
top_n(20, years)
  # 
# Analysis by year - nacionalidad pitching (resular season) ----

pit_nacionalidad <- prs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(years, nacionalidad) %>%
  summarise(
    pitcher = n_distinct(jugador),
    w = sum(w, na.rm = TRUE),
    l = sum(l, na.rm = TRUE),
    # g = sum(g, na.rm = TRUE),
    gs = sum(gs, na.rm = TRUE),
    cg = sum(cg, na.rm = TRUE),
    sho = sum(sho, na.rm = TRUE),
    sv = sum(sv, na.rm = TRUE),
    ip = sum(IP(ip), na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    er = sum(er, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    # era = (ip / er) * 9,
    era = round((er / IP(ip)) * 9, 3),
    whip = round((bb + h) / IP(ip), 3),
    `h/9` = round((h / IP(ip)) * 9, 2),
    `hr/9` = round((hr / IP(ip)) * 9, 2),
    `bb/9` = round((bb / IP(ip)) * 9, 2),
    `so/9` = round((so / IP(ip)) * 9, 2),
    `so/bb` = round(so / bb, 2),
    .groups = "drop"
  ) %>% 
  dplyr::ungroup() %>%
  top_n(20, years) %>% 
  filter(nacionalidad == "Venezolano")
  
# Analysis by country pitching (resular season) ----

pit_pais <- prs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(pais) %>%
  summarise(
    pitcher = n_distinct(jugador),
    w = sum(w, na.rm = TRUE),
    l = sum(l, na.rm = TRUE),
    # g = sum(g, na.rm = TRUE),
    gs = sum(gs, na.rm = TRUE),
    cg = sum(cg, na.rm = TRUE),
    sho = sum(sho, na.rm = TRUE),
    sv = sum(sv, na.rm = TRUE),
    ip = sum(IP(ip), na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    er = sum(er, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    # era = (ip / er) * 9,
    era = round((er / IP(ip)) * 9, 3),
    whip = round((bb + h) / IP(ip), 3),
    `h/9` = round((h / IP(ip)) * 9, 2),
    `hr/9` = round((hr / IP(ip)) * 9, 2),
    `bb/9` = round((bb / IP(ip)) * 9, 2),
    `so/9` = round((so / IP(ip)) * 9, 2),
    `so/bb` = round(so / bb, 2),
    .groups = "drop"
  ) %>% 
  dplyr::ungroup() 

# top_n(20, years)
  
# Analysis by decade batting (regular season) ----

decades_name <- as.factor(c("60","70", "80", "90", "00", "10", "20"))


bat_decade <- brs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(decade) %>%
  summarise(
    seasons = n_distinct(years),
    pa = sum(pa, na.rm = TRUE),
    ab = sum(ab, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    `2b` = sum(`2b`, na.rm = TRUE),
    `3b` = sum(`3b`, na.rm = TRUE),
    `hr` = sum(`hr`, na.rm = TRUE),
    rbi = sum(rbi, na.rm = TRUE),
    sb = sum(sb, na.rm = TRUE),
    cs = sum(cs, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    th = sum(`2b`, `3b`, hr, h, na.rm = TRUE),
    avg = round(th / ab, 3),
    obp = round(sum(h, bb, hbp, na.rm = TRUE) / sum(ab, bb, hbp, sh, na.rm = TRUE), 3),
    slg = round(sum(h, `2b` * 2, `3b` * 3, `hr` * 4) / ab, 3),
    ops = obp + slg,
    tb = sum(tb, na.rm = TRUE),
    xb = sum(xb, na.rm = TRUE),
    hbp = sum(hbp, na.rm = TRUE),
    sh = sum(sh, na.rm = TRUE),
    sf = sum(sf, na.rm = TRUE),
    .groups = "drop"
  ) 


%>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    # decade = as.factor(decade),
    decade = forcats::fct_relevel(
      decade, "60","70", "80", "90", "00", "10", "20"
    )
  ) %>% 
  arrange(decade)


# group_by(years, nacionalidad) %>%
# dplyr::filter(nacionalidad == "Importad") %>% 
top_n(20, years)
# 

# Analysis by year - nacionalidad (regular season) ----

decades_name <- as.factor(c("60","70", "80", "90", "00", "10", "20"))


bat_decade_nat<- brs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(decade, nacionalidad) %>%
  summarise(
    seasons = n_distinct(years),
    pa = sum(pa, na.rm = TRUE),
    ab = sum(ab, na.rm = TRUE),
    r = sum(r, na.rm = TRUE),
    h = sum(h, na.rm = TRUE),
    `2b` = sum(`2b`, na.rm = TRUE),
    `3b` = sum(`3b`, na.rm = TRUE),
    `hr` = sum(`hr`, na.rm = TRUE),
    rbi = sum(rbi, na.rm = TRUE),
    sb = sum(sb, na.rm = TRUE),
    cs = sum(cs, na.rm = TRUE),
    bb = sum(bb, na.rm = TRUE),
    so = sum(so, na.rm = TRUE),
    th = sum(`2b`, `3b`, hr, h, na.rm = TRUE),
    avg = round(th / ab, 3),
    obp = round(sum(h, bb, hbp, na.rm = TRUE) / sum(ab, bb, hbp, sh, na.rm = TRUE), 3),
    slg = round(sum(h, `2b` * 2, `3b` * 3, `hr` * 4) / ab, 3),
    ops = obp + slg,
    tb = sum(tb, na.rm = TRUE),
    xb = sum(xb, na.rm = TRUE),
    hbp = sum(hbp, na.rm = TRUE),
    sh = sum(sh, na.rm = TRUE),
    sf = sum(sf, na.rm = TRUE),
    .groups = "drop"
  ) 



# 

# Players debutantes by season ----
debutantes <- roster %>% 
  dplyr::filter(
    ronda == "regular",
    pais == "Venezuela",
    exp == 1
    ) %>% 
  dplyr::group_by(years) %>% 
  dplyr::summarise(
    
  )

%>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular"), 
                   by = c("player_id", "years", "jugador")) %>% 
  dplyr::select(-ronda.y) %>% 
  group_by(decade) 
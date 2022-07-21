library(dplyr)
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
  dplyr::ungroup() 

# top_n(20, years)
  
# Analysis by countrypitching (resular season) ----

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
  
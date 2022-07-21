library(dplyr)

source("Dataset.R")

# Analysis by decade pitching  (resular season) ----

pit_decade <- prs %>% 
  dplyr::filter(ronda == "regular") %>% 
  dplyr::left_join(roster %>% 
                     dplyr::filter(ronda == "regular") %>% 
                     dplyr::select(-player_id), by = c("years", "jugador"))
  
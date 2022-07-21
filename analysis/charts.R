library(ggplot)
library(dplyr)

# Charts 
# Carreras fabricadas por inning ----
anotadas <- box_score
anotadas <- dplyr::filter(anotadas, !is.na(Fecha))
anotadas <- dplyr::select(anotadas, Fecha, T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9)
anotadas <- dplyr::mutate(anotadas, T_9 = unlist(as.numeric(T_9)))
anotadas <- dplyr::rename(anotadas, 
                          "1" = T_1,  "2" = T_2, 
                          "3" = T_3,  "4" = T_4, 
                          "5" = T_5,  "6" = T_6, 
                          "7" = T_7, "8" = T_8, 
                          "9" = T_9)

anotadas <- tidyr::pivot_longer(anotadas, !Fecha, names_to = "innings")
anotadas <- dplyr::group_by(anotadas, innings)
anotadas <- dplyr::summarise(anotadas, carreras = sum(value, na.rm = T))




# Carreras recibidas por inning ----
permitidas <- box_score
permitidas <- dplyr::filter(permitidas, !is.na(Fecha))
permitidas <- dplyr::select(permitidas, Fecha, O_1, O_2, O_3, O_4, O_5, O_6, O_7, O_8, O_9)
permitidas <- dplyr::mutate(permitidas, O_9 = unlist(as.numeric(O_9)))
permitidas <- dplyr::rename(permitidas, 
                            "1" = O_1,  "2" = O_2, 
                            "3" = O_3,  "4" = O_4, 
                            "5" = O_5,  "6" = O_6, 
                            "7" = O_7,  "8" = O_8, 
                            "9" = O_9)

permitidas <- tidyr::pivot_longer(permitidas, !Fecha, names_to = "innings")
permitidas <- dplyr::group_by(permitidas, innings)
permitidas <- dplyr::summarise(permitidas, carreras = sum(value, na.rm = T))


# Graficos de permitidas  y anotadas ----
permitidas %>% 
  hchart(.,
         type = 'column',
         hcaes(x = innings,
               y = carreras),
         color = "#019fcb") %>%
  hc_title(
    # text = "Cases confirmed of <span style=\"color:#cc0000\"> CORONAVIRUS</span> in China",
    text = " ",
    useHTML = TRUE) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_tooltip(sort = TRUE,
             pointFormat = paste('<br><b>Carreras: {point.carreras:.1f}')) %>%
  hc_xAxis(title = list(text = "Innings")) %>%
  hc_yAxis(title = list(text = "Carreras"))


anotadas %>% 
  hchart(.,
         type = 'column',
         hcaes(x = innings,
               y = carreras),
         color = "#0d3583") %>%
  hc_title(
    # text = "Cases confirmed of <span style=\"color:#cc0000\"> CORONAVIRUS</span> in China",
    text = " ",
    useHTML = TRUE) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_tooltip(sort = TRUE,
             pointFormat = paste('<br><b>Carreras: {point.carreras:.1f}')) %>%
  hc_xAxis(title = list(text = "Innings")) %>%
  hc_yAxis(title = list(text = "Carreras"))


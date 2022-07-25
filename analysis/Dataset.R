# Roster ----
.Rosters <- readr::read_csv('data/rosters_clean.csv')

.rosters <- .Rosters %>%
  dplyr::arrange(jugador, years) %>%
  dplyr::distinct(name) %>%
  dplyr::mutate(ID =  paste(substr(name, 1, 1), seq(1, length(name), 1) , sep = '')
                )

roster <- .Rosters %>%
  dplyr::arrange(jugador, years) %>%
  dplyr::left_join(.rosters, by = 'name') %>% 
  dplyr::mutate(
    key = paste0(as.character(years), jugador),
    f_nac = as.Date(f_nac, format = "%m/%d/%Y"),
    nacionalidad = ifelse(pais == "Venezuela", "Venezolano", "Importado")
    )

# Batting -----
brs <- readr::read_csv('data/batting_reseason.csv') %>% 
  dplyr::mutate(
    decade = dplyr::case_when(
      years %in% c("1962-63", "1963-64", "1964-65", "1965-66", "1966-67", "1967-68", "1968-69", "1969-70") ~ "60",
      years %in% c("1970-71", "1971-72", "1972-73", "1973-74", "1974-75", "1975-76", "1976-77", "1977-78", "1978-79", 
                   "1979-80") ~ "70",
      years %in% c("1980-81", "1981-82", "1982-83", "1983-84", "1984-85", "1985-86", "1986-87", "1987-88", "1988-89", 
                   "1989-90") ~ "80",
      years %in% c("1990-91", "1991-92", "1992-93", "1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", 
                   "1999-00") ~ "90",
      years %in% c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", 
                   "2009-10") ~ "00",
      years %in% c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
                   "2019-20") ~ "10",
      years %in% c("2020-21", "2021-22", "2022-23") ~ "20"
      )
    )
  

# Pitching ----
prs <- readr::read_csv('data/pitching_reseason.csv') %>% 
  dplyr::mutate(
    decade = dplyr::case_when(
      years %in% c("1962-63", "1963-64", "1964-65", "1965-66", "1966-67", "1967-68", "1968-69", "1969-70") ~ "60",
      years %in% c("1970-71", "1971-72", "1972-73", "1973-74", "1974-75", "1975-76", "1976-77", "1977-78", "1978-79", 
                   "1979-80") ~ "70",
      years %in% c("1980-81", "1981-82", "1982-83", "1983-84", "1984-85", "1985-86", "1986-87", "1987-88", "1988-89", 
                   "1989-90") ~ "80",
      years %in% c("1990-91", "1991-92", "1992-93", "1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", 
                   "1999-00") ~ "90",
      years %in% c("2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", 
                   "2009-10") ~ "00",
      years %in% c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
                   "2019-20") ~ "10",
      years %in% c("2020-21", "2021-22", "2022-23") ~ "20"
    )
  )


# # Sheets connect Box Score ----
# .sheet <- "hhttps://docs.google.com/spreadsheets/d/1Jcrj02LE8KO_T_KJALeqz3ziESLirlKWQfd-IpDrXwQ/edit#gid=0"
# 
# 
# futile.logger::flog.info("Getting  Box Score")
# box_score <- googlesheets4::read_sheet(.sheet, sheet = "Box Score")
# box_score <- dplyr::mutate(box_score, Fecha = as.Date(Fecha, format = "%Y-%m-%d"))
# 
# 

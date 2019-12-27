#Libraries ----
library(dplyr)
library(rvest)
library(futile.logger)


# Connect to Pelota binaria ----
#Page year 1962-1963
.url <- 'http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=1962-63'

#vector of years statistics baseball 
years <- c('1962-63')

#url with all years
.tgl_page <- paste('http://www.pelotabinaria.com.ve/beisbol/tem_equ.php?EQ=TIB&TE=', years, sep = "" )


library(readr)
library(tidyverse)
idesi <- read_delim("INPUT/DATA/Desigualdad (S80_S20) (CCAA).csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols(
                     `﻿Territorio`= readr::col_factor(levels = NULL)))n
desi<-
  desi %>% 
    mutate(ComunidadAutonoma = factor(`﻿Territorio`, levels = c("Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla", "España")))

desi<-
desi %>% 
  filter(ComunidadAutonoma != "España") %>% 
  droplevels()

levels(desi$ComunidadAutonoma)

desi<-
  desi %>% 
  filter(`Año/curso académico` == 2019)

 <- read_delim("INPUT/DATA/47444.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) %> 
col_types = cols(
                    `Comunidades y Ciudades Autónomas` = readr::col_factor(levels = NULL))i
ina<-
  ina %>% 
  mutate(ComunidadAutonoma = factor(`Comunidades y Ciudades Autónomas`, labels = c( "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla")))


 <- read_delim("INPUT/DATA/02001bsc.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(encoding="latin1"),
 , col_types = cols(
                    `Comunidad y ciudad autónoma de residencia` = readr::col_factor(levels = NULL)))desui<-
  sui %>% 
  mutate(ComunidadAutonoma = factor(`Comunidad y ciudad autónoma de residencia`, labels = c( "Total", "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla", "Extranjero")))s
sui<-
  sui %>% 
  filter(ComunidadAutonoma != "Total") %>% 
  filter(ComunidadAutonoma != "Extranjero") %>% 
  droplevels()

levels(sui$ComunidadAutonoma)

els(desfactor(sui$`Comunidad y ciudad autónoma de residencia`i)ew(ina)
View(sui)

sui <- 
  sui%>%
  mutate()
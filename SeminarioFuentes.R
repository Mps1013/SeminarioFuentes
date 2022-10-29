library(readr)
library(tidyverse)

desi <- read_delim("INPUT/DATA/Desigualdad (S80_S20) (CCAA).csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols(
                     `﻿Territorio`= readr::col_factor(levels = NULL)))


print(desi)
print(ina)
print(sui)

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

ina <- read_delim("INPUT/DATA/47444.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) 
col_types = cols(
                    `Comunidades y Ciudades Autónomas` = readr::col_factor(levels = NULL))

ina<-
  ina %>% 
  mutate(ComunidadAutonoma = factor(`Comunidades y Ciudades Autónomas`,levels = c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)", "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)"), labels = c( "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla")))

levels(ina$ComunidadAutonoma)

sui <- read_delim("INPUT/DATA/02001bsc.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(encoding="latin1"), col_types = cols(
                    `Comunidad y ciudad autónoma de residencia` = readr::col_factor(levels = NULL)))

sui<-
  sui %>% 
  mutate(ComunidadAutonoma = factor(`Comunidad y ciudad autónoma de residencia`, labels = c( "Total", "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla", "Extranjero")))

sui<-
  sui %>% 
  filter(ComunidadAutonoma != "Total") %>% 
  filter(ComunidadAutonoma != "Extranjero") %>% 
  droplevels()

levels(sui$ComunidadAutonoma)

#Tabla suicidios y desigualdad.

sude <- left_join(x = sui, y = desi, by = c("ComunidadAutonoma"))

View (sude)

sude <- select(.data = sude, Total, ComunidadAutonoma, value)

sude <- rename(.data = sude, c(TotalSuicidios = "Total"))

sude <- rename(.data = sude, c(s80s20= "value"))

sude <- relocate(.data = sude, ComunidadAutonoma, .before = TotalSuicidios)


#Tabla suicidios e innacesibilidad.

suina <- left_join(x = sui, y = ina, by = c("ComunidadAutonoma"))

View(suina)

suina <- rename(.data = suina, c(TotalSuicidios = "Total.x"))

suina <- rename(.data = suina, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

suina <- rename(.data = suina, c(AsistenciaSanitaria = "Sí o no"))

suina <- rename(.data = suina, c(TotalInaccesibilidad = "Total.y"))

suina <- select(.data = suina, TotalSuicidios, ComunidadAutonoma,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad)

suina <- relocate(.data = suina, ComunidadAutonoma, .before = TotalSuicidios)

#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

table(suina$TipoAtencionSanitaria)
print(suina$TipoAtencionSanitaria)

suina <- filter(suina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

table(suina$AsistenciaSanitaria)
print(suina$AsistenciaSanitaria)

suina <- filter(suina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

suina <- filter(suina, Sexo.y == "Ambos sexos")

#Tabla suina

suina <- select(.data = suina, TotalSuicidios, ComunidadAutonoma, TotalInaccesibilidad)

suina <- relocate(.data = suina, ComunidadAutonoma, .before = TotalSuicidios)

#Tabla desigualdad e inaccesibilidad

desina <- left_join(x = desi, y = ina, by = c("ComunidadAutonoma"))
View(desina)

desina <- rename(.data = desina, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

desina <- rename(.data = desina, c(AsistenciaSanitaria = "Sí o no"))

desina <- rename(.data = desina, c(TotalInaccesibilidad = "Total"))

desina <- rename(.data = desina, c(s80s20= "value"))

#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

print(desina$TipoAtencionSanitaria)

desina <- filter(desina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

print(desina$AsistenciaSanitaria)

desina <- filter(desina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

desina <- filter(desina, Sexo == "Ambos sexos")

#Tabla desina

desina <- select(.data = desina, s80s20, ComunidadAutonoma, TotalInaccesibilidad)

desina <- relocate(.data = desina, ComunidadAutonoma, .before = s80s20)

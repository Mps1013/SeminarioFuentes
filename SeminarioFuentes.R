library(readr)
library(tidyverse)

desi <- read_delim("INPUT/DATA/Desigualdad (S80_S20) (CCAA).csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols(
                     `﻿Territorio`= readr::col_factor(levels = NULL)))

desi<-
  desi %>% 
  mutate(CA = factor(`﻿Territorio`, levels = c("Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla", "España")))

desi<-
  desi %>% 
  filter(CA != "España") %>% 
  droplevels()

#levels(desi$CA)

desi<-
  desi %>% 
  filter(`Año/curso académico` == 2019)

ina <- read_delim("INPUT/DATA/47444.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) 
col_types = cols(
  `Comunidades y Ciudades Autónomas` = readr::col_factor(levels = NULL))

ina<-
  ina %>% 
  mutate(CA = factor(`Comunidades y Ciudades Autónomas`,levels = c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)", "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)"), labels = c( "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla")))

#levels(ina$CA)

sui <- read_delim("INPUT/DATA/02001bsc.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(encoding="latin1"), col_types = cols(
                    `Comunidad y ciudad autónoma de residencia` = readr::col_factor(levels = NULL)))

sui<-
  sui %>% 
  mutate(CA = factor(`Comunidad y ciudad autónoma de residencia`, labels = c( "Total", "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla", "Extranjero")))

sui<-
  sui %>% 
  filter(CA != "Total") %>% 
  filter(CA != "Extranjero") %>% 
  droplevels()

#levels(sui$CA)

pob <- read_delim("INPUT/DATA/2853bsc.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE,locale=locale(encoding="latin1"), col_types = cols(
                    `Comunidades y Ciudades Autónomas` = readr::col_factor(levels = NULL)))
pob<-
  pob %>% 
  mutate(CA = factor(`Comunidades y Ciudades Autónomas`, labels = c("Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla")))
levels(pob$CA)
View(pob)
#Tabla suicidios y desigualdad, población.

sude <- left_join(x = sui, y = desi, by = c("CA"))

sude <- left_join(x = sude, y = pob, by = c("CA"))

View(sude)

sude <- select(.data = sude, Total.x, CA, value, Total.y)

sude <- rename(.data = sude, c(TotalSuicidios = "Total.x"))

sude <- rename(.data = sude, c(TotalPoblacion = "Total.y"))

sude <- rename(.data = sude, c(s80s20= "value"))

sude <- relocate(.data = sude, CA, .before = TotalSuicidios)

#Creo nueva columna

sude$TotalPoblacion <- as.numeric(sude$TotalPoblacion)

sude$porPob <- (sude$TotalPoblacion*100) / (47026208)

#Tabla suicidios e innacesibilidad.

suina <- left_join(x = sui, y = ina, by = c("CA"))

suina <- left_join(x = suina, y = pob, by = c("CA"))

View(suina)

suina <- rename(.data = suina, c(TotalSuicidios = "Total.x"))

suina <- rename(.data = suina, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

suina <- rename(.data = suina, c(AsistenciaSanitaria = "Sí o no"))

suina <- rename(.data = suina, c(TotalInaccesibilidad = "Total.y"))

suina <- rename(.data = suina, c(TotalPoblacion = "Total"))

suina <- select(.data = suina, TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion)

suina <- relocate(.data = suina, CA, .before = TotalSuicidios)


#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

#table(suina$TipoAtencionSanitaria)

suina <- filter(suina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

#table(suina$AsistenciaSanitaria)

suina <- filter(suina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

suina <- filter(suina, Sexo.y == "Ambos sexos")

#Tabla suina

suina <- select(.data = suina, TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)

suina <- relocate(.data = suina, CA, .before = TotalSuicidios)


view(suina)
#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
suina$TotalInaccesibilidad<-as.numeric(gsub(',', '.',desina$TotalInaccesibilidad))

#Tabla desigualdad e inaccesibilidad

desina <- left_join(x = desi, y = ina, by = c("CA"))

desina <- left_join(x = desina, y = pob, by = c("CA"))

View(desina)

desina <- rename(.data = desina, c(TotalPoblacion = "Total.y"))

desina <- rename(.data = desina, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

desina <- rename(.data = desina, c(AsistenciaSanitaria = "Sí o no"))

desina <- rename(.data = desina, c(TotalInaccesibilidad = "Total.x"))

desina <- rename(.data = desina, c(s80s20= "value"))

#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

#table(desina$TipoAtencionSanitaria)

desina <- filter(desina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

#table(desina$AsistenciaSanitaria)

desina <- filter(desina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

desina <- filter(desina, Sexo.x == "Ambos sexos")

#Tabla desina

desina <- select(.data = desina, s80s20, CA, TotalInaccesibilidad, TotalPoblacion)

desina <- relocate(.data = desina, CA, .before = s80s20)

#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
desina$TotalInaccesibilidad<-as.numeric(gsub(',', '.',desina$TotalInaccesibilidad))

#Todas las tablas
sude
suina
desina



#SUDE
#Se preparan los datos para graficar
sudeG <- pivot_longer(sude,-CA , names_to="variable", values_to="value")

#Gráfica sude
ggplot(sudeG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()


#SUINA
#Se preparan los datos para graficar
suinaG <- pivot_longer(suina,-CA , names_to="variable", values_to="value")

#Gráfica suina
ggplot(suinaG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()

#DESINA
#Se preparan los datos para graficar
desinaG <- pivot_longer(desina,-CA , names_to="variable", values_to="value")

#Gráfica desina
ggplot(desinaG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()
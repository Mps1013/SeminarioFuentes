library(readr)
library(tidyverse)

desi <- read_delim("INPUT/DATA/desigualdad.csv",
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

ina <- read_delim("INPUT/DATA/inaccesibilidad.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) 
col_types = cols(
  `Comunidades y Ciudades Autónomas` = readr::col_factor(levels = NULL))

ina<-
  ina %>% 
  mutate(CA = factor(`Comunidades y Ciudades Autónomas`,levels = c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)", "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)", "Ceuta (Ciudad Autónoma de)", "Melilla (Ciudad Autónoma de)"), labels = c( "Andalucía", "Aragón", "Asturias", "Baleares, Islas", "Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid, Comunidad de", "Murcia, Región de", "Navarra", "País Vasco", "Rioja, La", "Ceuta", "Melilla")))

#levels(ina$CA)

sui <- read_delim("INPUT/DATA/suicidios.csv",
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

pob <- read_delim("INPUT/DATA/poblacion.csv",
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

sude$porPobSui <- (sude$TotalSuicidios*100) / sude$TotalPoblacion
sude

#Tabla suicidios e innacesibilidad.

suina <- left_join(x = sui, y = ina, by = c("CA"))

suina <- left_join(x = suina, y = pob, by = c("CA"))

#View(suina)

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


#view(suina)

#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
suina$TotalInaccesibilidad<-as.numeric(gsub(',', '.',suina$TotalInaccesibilidad))
suina$porPobSui <- (suina$TotalSuicidios*100) / suina$TotalPoblacion
suina$porPobIna <- (suina$TotalInaccesibilidad*100) / suina$TotalPoblacion




suina
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
desina$porPobIna <- (desina$TotalInaccesibilidad*100) / desina$TotalPoblacion
#desina$CA<-sort(desina$CA)



#Todas las tablas
sude
suina
desina



#SUDE
#Se preparan los datos para graficar
sudeG<-select(.data=sude,"CA","s80s20","porPobSui")
sudeG
sudeG <- pivot_longer(sudeG,-CA , names_to="variable", values_to="value")
sude
#Gráfica sude
ggplot(sudeG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()

#Regresión lineal sude
ggplot(data = sude, aes(x = porPobSui, y = s80s20)) +
  geom_point(aes(colour = factor(CA))) +
  labs(title = 'Relación entre Suicidios y Desigualdad', subtitle = 'Regresión Lineal', x = "Suicidios/Población", y = "Renta (s80s20)", colour = "Comunidades y Ciudades Autónomas" )

#SUINA
#Se preparan los datos para graficar
suinaG<-select(.data=suina, "CA","porPobIna","porPobSui") 
suinaG
suinaG <- pivot_longer(suinaG,-CA , names_to="variable", values_to="value")
#Gráfica suina
ggplot(suinaG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()

#Regresión lineal suina
ggplot(data = suina, aes(x = porPobSui, y = porPobIna)) +
  geom_point(aes(colour = factor(CA)))+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Salud Mental
       Regresión Lineal', x = "Suicidios/Población", y = "Inaccesibilidad/Población", colour = "Comunidades y Ciudades Autónomas")


#DESINA
#Se preparan los datos para graficar
desina
desinaG<-select(.data=desina,"CA","s80s20","porPobIna")
desinaG <- pivot_longer(desinaG,-CA , names_to="variable", values_to="value")

#Gráfica desina
ggplot(desinaG,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()



plot(desina$s80s20, desina$porPobIna, xlab='s20s80', ylab='Inaccesibilidad')
abline(regresion)


ggplot(data = desina, aes(x = s80s20, y = porPobIna)) +
  geom_point(aes(colour = factor(CA))) +
  labs(title = 'Relación entre Desigualdad e Inaccesibilidad', subtitle = 'Regresión Lineal')


ggplot(data = desina, aes(x = s80s20, y = porPobIna)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red")


#Suina 2

#Tabla suicidios e innacesibilidad por (Atención médica).

suina2 <- left_join(x = sui, y = ina, by = c("CA"))

suina2 <- left_join(x = suina2, y = pob, by = c("CA"))

#View(suina)

suina2 <- rename(.data = suina2, c(TotalSuicidios = "Total.x"))

suina2 <- rename(.data = suina2, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

suina2 <- rename(.data = suina2, c(AsistenciaSanitaria = "Sí o no"))

suina2 <- rename(.data = suina2, c(TotalInaccesibilidad = "Total.y"))

suina2 <- rename(.data = suina2, c(TotalPoblacion = "Total"))

suina2 <- select(.data = suina2, TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion)

suina2 <- relocate(.data = suina2, CA, .before = TotalSuicidios)

View(suina2)
#Escoge solo "Atención médica"

#table(suina2$TipoAtencionSanitaria)

suina2 <- filter(suina2, TipoAtencionSanitaria == "Atención médica")

#Escoge Sí, los que no hay recibido atención médica

#table(suina$AsistenciaSanitaria)

suina2 <- filter(suina2, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

suina2 <- filter(suina2, Sexo.y == "Ambos sexos")

#Tabla suina2

suina2 <- select(.data = suina2, TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)

suina2 <- relocate(.data = suina2, CA, .before = TotalSuicidios)

#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
suina2$TotalInaccesibilidad<-as.numeric(gsub(',', '.',suina2$TotalInaccesibilidad))
suina2$porPobSui <- (suina2$TotalSuicidios*100) / suina2$TotalPoblacion
suina2$porPobIna <- (suina2$TotalInaccesibilidad*100) / suina2$TotalPoblacion

suina

#SUINA2
#Se preparan los datos para graficar
suina2G<-select(.data=suina2, "CA","porPobIna","porPobSui") 
suina2G
suina2G <- pivot_longer(suina2G,-CA , names_to="variable", values_to="value")
#Gráfica suina2
ggplot(suina2G,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()

#Regresión lineal suina
ggplot(data = suina2, aes(x = porPobSui, y = porPobIna)) +
  geom_point(aes(colour = factor(CA)))+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Atención Médica
       Regresión Lineal')

#Suina 3

#Tabla suicidios e innacesibilidad por (Medicamento recetado).

suina3 <- left_join(x = sui, y = ina, by = c("CA"))

suina3 <- left_join(x = suina3, y = pob, by = c("CA"))

suina3 <- rename(.data = suina3, c(TotalSuicidios = "Total.x"))

suina3 <- rename(.data = suina3, c(TipoAtencionSanitaria = "Tipos atención sanitaria"))

suina3 <- rename(.data = suina3, c(AsistenciaSanitaria = "Sí o no"))

suina3 <- rename(.data = suina3, c(TotalInaccesibilidad = "Total.y"))

suina3 <- rename(.data = suina3, c(TotalPoblacion = "Total"))

suina3 <- select(.data = suina3, TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion)

suina3 <- relocate(.data = suina3, CA, .before = TotalSuicidios)

View(suina3)
#Escoge solo "Atención médica"

#table(suina3$TipoAtencionSanitaria)

suina3 <- filter(suina3, TipoAtencionSanitaria == "Medicamento recetado")

#Escoge Sí, los que no hay recibido atención médica

#table(suina$AsistenciaSanitaria)

suina3 <- filter(suina3, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

suina3 <- filter(suina3, Sexo.y == "Ambos sexos")

#Tabla suina3

suina3 <- select(.data = suina3, TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)

suina3 <- relocate(.data = suina3, CA, .before = TotalSuicidios)

#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
suina3$TotalInaccesibilidad<-as.numeric(gsub(',', '.',suina3$TotalInaccesibilidad))
suina3$porPobSui <- (suina3$TotalSuicidios*100) / suina3$TotalPoblacion
suina3$porPobIna <- (suina3$TotalInaccesibilidad*100) / suina3$TotalPoblacion

suina3

#SUINA3
#Se preparan los datos para graficar
suina3G<-select(.data=suina3, "CA","porPobIna","porPobSui") 
suina3G
suina3G <- pivot_longer(suina3G,-CA , names_to="variable", values_to="value")
#Gráfica suina2
ggplot(suina3G,aes(x = CA, y=value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
  theme(axis.text = element_text(angle = 90))+
  scale_y_continuous()

#Regresión lineal suina
ggplot(data = suina3, aes(x = porPobSui, y = porPobIna)) +
  geom_point(aes(colour = factor(CA)))+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Medicamento Recetado
       Regresión Lineal')

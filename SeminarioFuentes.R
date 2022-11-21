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

sude <- 
  sude %>%
    select(.data = ., Total.x, CA, value, Total.y) %>%
    rename(c(TotalSuicidios = "Total.x")) %>%
    rename(c(TotalPoblacion = "Total.y")) %>%
    rename(c(s80s20= "value")) %>%
    relocate(CA, .before = TotalSuicidios)
#Creo nueva columna

sude$TotalPoblacion <- as.numeric(sude$TotalPoblacion)

sude$porPobSui <- (sude$TotalSuicidios*100) / sude$TotalPoblacion
sude

#Tabla suicidios e innacesibilidad.

suina <- left_join(x = sui, y = ina, by = c("CA"))

suina <- left_join(x = suina, y = pob, by = c("CA"))

#View(suina)

suina <- 
  suina %>%
  rename(.data = ., c(TotalSuicidios = "Total.x")) %>%
  rename(c(TipoAtencionSanitaria = "Tipos atención sanitaria")) %>%
  rename(c(AsistenciaSanitaria = "Sí o no")) %>%
  rename(c(TotalInaccesibilidad = "Total.y")) %>%
  rename(c(TotalPoblacion = "Total")) %>%
  select(TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion) %>%
  relocate(CA, .before = TotalSuicidios)
suina

#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

#table(suina$TipoAtencionSanitaria)

suina <-
  suina %>%
  filter(TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)") %>%
  filter(AsistenciaSanitaria == "Sí") %>%
  filter(Sexo.y == "Ambos sexos") %>%
  select(TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion) %>%
  relocate(CA, .before = TotalSuicidios)

suina 

#suina <- filter(suina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

#table(suina$AsistenciaSanitaria)

#suina <- filter(suina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

#suina <- filter(suina, Sexo.y == "Ambos sexos")

#Tabla suina

#suina <- select(.data = suina, TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)

#suina <- relocate(.data = suina, CA, .before = TotalSuicidios)

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

desina <- 
  desina %>%
  rename(c(TotalPoblacion = "Total.y")) %>%
  rename(c(TipoAtencionSanitaria = "Tipos atención sanitaria")) %>% 
  rename(c(AsistenciaSanitaria = "Sí o no")) %>%
  rename(c(TotalInaccesibilidad = "Total.x")) %>%
  rename(c(s80s20= "value")) %>%
  filter(TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)") %>%
  filter(AsistenciaSanitaria == "Sí") %>%
  filter(Sexo.x == "Ambos sexos") %>%
  select(s80s20, CA, TotalInaccesibilidad, TotalPoblacion) %>%
  relocate(CA, .before = s80s20)

desina 

#Escoge solo "Atención salud mental (psicólogo, psiquiatra...)"

#table(desina$TipoAtencionSanitaria)
#desina <- filter(desina, TipoAtencionSanitaria == "Atención salud mental (psicólogo, psiquiatra...)")

#Escoge Sí, los que no hay recibido atención médica

#table(desina$AsistenciaSanitaria)

#desina <- filter(desina, AsistenciaSanitaria == "Sí")

#Escoge ambos sexos

#desina <- filter(desina, Sexo.x == "Ambos sexos")

#Tabla desina

#desina <- select(.data = desina, s80s20, CA, TotalInaccesibilidad, TotalPoblacion)

#desina <- relocate(.data = desina, CA, .before = s80s20)

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
sude_plot1 <-
  ggplot(sudeG,aes(x = CA, y=value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
    theme(axis.text = element_text(angle = 90))+
    scale_y_continuous()+
    theme_light()

#Regresión lineal sude
sude_plot2 <-
  ggplot(data = sude, aes(x = s80s20, y = porPobSui)) +
    geom_point(aes(colour = factor(CA))) +
    geom_smooth(method = "lm", colour = "blue")+
    labs(title = 'Relación entre Suicidios y Desigualdad', subtitle = 'Regresión Lineal', x = "Suicidios/Población", y = "Renta (s80s20)", colour = "Comunidades y Ciudades Autónomas" )+
    theme_light()

#SUINA
#Se preparan los datos para graficar
suinaG<-select(.data=suina, "CA","porPobIna","porPobSui") 
suinaG
suinaG <- pivot_longer(suinaG,-CA , names_to="variable", values_to="value")
#Gráfica suina
suina_plot1 <-
  ggplot(suinaG,aes(x = CA, y=value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
    theme(axis.text = element_text(angle = 90))+
    scale_y_continuous()+
    theme_light()

#Regresión lineal suina
suina_plot2 <-
  ggplot(data = suina, aes(x = porPobSui, y = porPobIna)) +
    geom_point(aes(colour = factor(CA)))+
    labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Salud Mental
         Regresión Lineal', x = "Suicidios/Población", y = "Inaccesibilidad/Población", colour = "Comunidades y Ciudades Autónomas")+
    theme_light()


#DESINA
#Se preparan los datos para graficar
desina
desinaG<-select(.data=desina,"CA","s80s20","porPobIna")
desinaG <- pivot_longer(desinaG,-CA , names_to="variable", values_to="value")

#Gráfica desina
desina_plot <-
  ggplot(desinaG,aes(x = CA, y=value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
    theme(axis.text = element_text(angle = 90))+
    scale_y_continuous()+
    theme_light()


desina_plot1 <-
  plot(desina$s80s20, desina$porPobIna, xlab='s20s80', ylab='Inaccesibilidad')
  abline(regresion)+
    theme_light()

desina_plot2 <-
  ggplot(data = desina, aes(x = s80s20, y = porPobIna)) +
    geom_point(aes(colour = factor(CA))) +
    labs(title = 'Relación entre Desigualdad e Inaccesibilidad', subtitle = 'Regresión Lineal', x = "Renta (s80s20)", y = "Innacesibilidad/Población", colour = "Comunidades y Ciudades Autónomas")+
    theme_light()

desina_plot3 <-
  ggplot(data = desina, aes(x = s80s20, y = porPobIna)) +
    geom_point() +
    geom_smooth(method = "lm", colour = "red")+
    theme_light()


#Suina 2

#Tabla suicidios e innacesibilidad por (Atención médica).
suina2 <- left_join(x = sui, y = ina, by = c("CA"))
suina2 <- left_join(x = suina2, y = pob, by = c("CA"))
suina2 <-
  suina2 %>%
  rename(c(TotalSuicidios = "Total.x"))%>%
  rename(c(TipoAtencionSanitaria = "Tipos atención sanitaria"))%>%
  rename(c(AsistenciaSanitaria = "Sí o no"))%>%
  rename(c(TotalInaccesibilidad = "Total.y"))%>%
  rename(c(TotalPoblacion = "Total"))%>%
  select(TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion)%>%
  relocate(CA, .before = TotalSuicidios)
#Escoge solo "Atención médica"
  
suina2 <-
  suina2 %>%
  filter(TipoAtencionSanitaria == "Atención médica")%>%
  #Escoge Sí, los que no hay recibido atención médica
  filter(AsistenciaSanitaria == "Sí")%>%
  #Escoge ambos sexos
  filter(Sexo.y == "Ambos sexos")%>%
  #Tabla suina2
  select(TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)%>%
  relocate(CA, .before = TotalSuicidios)

#Se pasan los chr a dbl para poder hacer los gráficos
#Se cambia el separador decimal "," por "." para poder hacer el cambio de tipo
suina2$TotalInaccesibilidad<-as.numeric(gsub(',', '.',suina2$TotalInaccesibilidad))
suina2$porPobSui <- (suina2$TotalSuicidios*100) / suina2$TotalPoblacion
suina2$porPobIna <- (suina2$TotalInaccesibilidad*100) / suina2$TotalPoblacion

suina2

#SUINA2
#Se preparan los datos para graficar
suina2G<-select(.data=suina2, "CA","porPobIna","porPobSui") 
suina2G
suina2G <- pivot_longer(suina2G,-CA , names_to="variable", values_to="value")
#Gráfica suina2
suina2_plot1 <-
  ggplot(suina2G,aes(x = CA, y=value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
    theme(axis.text = element_text(angle = 90))+
    scale_y_continuous()
print(suina2_plot1)

#Regresión lineal suina2
suina2_plot2 <-
  ggplot(data = suina2, aes(x = porPobSui, y = porPobIna)) +
    geom_point(aes(colour = factor(CA)))+
    labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Atención Médica
         Regresión Lineal', x = "Suicidios/Poblacion", y = "Innacesibilidad/Población", colour = "Comunidades y Ciudades Autónomas")+
    theme_light()
print(suina2_plot2)

#Suina 3

#Tabla suicidios e innacesibilidad por (Medicamento recetado).

suina3 <- left_join(x = sui, y = ina, by = c("CA")) 
suina3 <- left_join(x = suina3, y = pob, by = c("CA")) 
suina3 <- 
  suina3 %>%
  rename(c(TotalSuicidios = "Total.x")) %>%
  rename(c(TipoAtencionSanitaria = "Tipos atención sanitaria")) %>%
  rename(c(AsistenciaSanitaria = "Sí o no"))%>%
  rename(c(TotalInaccesibilidad = "Total.y"))%>%
  rename(c(TotalPoblacion = "Total"))%>%
  select(TotalSuicidios, CA,Sexo.y, TipoAtencionSanitaria, AsistenciaSanitaria, TotalInaccesibilidad, TotalPoblacion)%>%
  relocate(CA, .before = TotalSuicidios)%>%
  #Escoge solo "Atención médica"
  filter(TipoAtencionSanitaria == "Medicamento recetado")%>%
  #Escoge Sí, los que no hay recibido atención médica
  filter(AsistenciaSanitaria == "Sí")%>%
  #Escoge ambos sexos
  filter(Sexo.y == "Ambos sexos")

#Tabla suina3

suina3 <- 
  suina3 %>%
  select(TotalSuicidios, CA, TotalInaccesibilidad, TotalPoblacion)%>%
  relocate(CA, .before = TotalSuicidios)

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
#Gráfica suina3
suina3_plot1 <-
  ggplot(suina3G,aes(x = CA, y=value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge" ) + 
    theme(axis.text = element_text(angle = 90))+
    scale_y_continuous()

#Regresión lineal suina3
suina3_plot2 <-
  ggplot(data = suina3, aes(x = porPobSui, y = porPobIna)) +
    geom_point(aes(colour = factor(CA)))+
    labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Medicamento Recetado
         Regresión Lineal', x = "Suicidios/Población", y = "Inaccesibilidad/Población", colour = "Comunidades y Ciudades Autónomas")+
    theme_light()

#Juntar las 3 gráficas de suina.
library(cowplot)

#Gráfica suina_plot2 sin leyenda
suina_plot2.1 <-
  ggplot(data = suina, aes(x = porPobSui, y = porPobIna)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "blue")+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Salud Mental
         Regresión Lineal', x = "Suicidios/Población", y = "Inaccesibilidad/Población")+
  theme_light()

#Gráfica suina2_plot2 sin leyenda

suina2_plot2.1 <-
  ggplot(data = suina2, aes(x = porPobSui, y = porPobIna)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "blue")+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Atención Médica
         Regresión Lineal', x = "Suicidios/Poblacion", y = "Innacesibilidad/Población")+
  theme_light()

#Gráfica suina3_2 sin leyenda

suina3_plot2.1 <-
  ggplot(data = suina3, aes(x = porPobSui, y = porPobIna)) +
  geom_point()+
  geom_smooth(method = "lm", colour = "blue")+
  labs(title = 'Relación entre Suicidios e Inaccesibilidad', subtitle = 'Por Medicamento Recetado
         Regresión Lineal', x = "Suicidios/Población", y = "Inaccesibilidad/Población")+
  theme_light()

plot_grid(suina_plot2.1, suina2_plot2.1, suina3_plot2.1, labels = c('A', 'B', 'C'), label_size = 10)


#Correlaciones: Hay una relación pero no se sabe si es causa y efecto. 
#desina_plot2 / inacc-renta
cor(desina$s80s20,desina$porPobIna)
#sude_plot2 / renta-sui
cor(sude$s80s20,sude$porPobSui)
#suina_plot2 / inacc salud mental - sui
cor(suina$porPobIna,suina$porPobSui)
#suina2_plot2  /inacc att med-sui
cor(suina2$porPobIna,suina2$porPobSui)
#suina3_plot2 / inacc receta -sui
cor(suina3$porPobSui, suina3$porPobIna)

#Modelos basados en regresión linel, causa-efecto.

#Modelo desina. Entre renta e innacesibilidad salud mental.
Modelo1 <- lm(porPobIna ~ s80s20, data = desina)
summary(Modelo1)

#Modelo sude. Entre renta y suicidios.
Modelo2 <- lm(s80s20 ~ porPobSui, data = sude)
summary(Modelo2)

#Modelo suina. Entre suicidios e innacesibilidad por salud mental.
Modelo3 <- lm(porPobIna ~ porPobSui, data = suina)
summary(Modelo3)

#Modelo suina. Entre suicidios e innacesibilidad por atencón médica.
Modelo4 <- lm(porPobIna ~ porPobSui, data = suina2)
summary(Modelo4)

#Modelo suina. Entre suicidios e innacesibilidad por medicamento recetado.
Modelo5 <- lm(porPobIna ~ porPobSui, data = suina3)
summary(Modelo5)

#Guardar en output cada gráfico creado.
#Gráficos de barras ponerlo bien.

library(ggplot2)
library(dplyr)
library(tidyverse)

vacunas <- vacunas[vacunas$Fecha_1_dosis != "",]

vacunas$Fecha_2_dosis <- as.Date(vacunas$Fecha_2_dosis, format = "%d/%m/%Y")
vacunas$Fecha_1_dosis <- as.Date(vacunas$Fecha_1_dosis, format = "%d/%m/%Y")

vacunas<-subset(vacunas,select = c(Fecha_1_dosis,Fecha_2_dosis))


attach(vacunas)

vac_mes<-data.frame(fecha = format(as.Date(Fecha_1_dosis), "%Y-%m"))
vac_mes$dosis <- "1era"

vac_mes_aux<-data.frame(fecha = format(as.Date(Fecha_2_dosis), "%Y-%m"))
vac_mes_aux$dosis <- "2da"

datos <- rbind(vac_mes, vac_mes_aux)
datos$dosis <- as.factor(datos$dosis)

cant_trabajadores <- 712

a<-datos %>% 
  filter(!is.na(fecha)) %>% 
  arrange(dosis, fecha) %>% 
  group_by(dosis, fecha) %>% 
  summarise(suma = n()) %>% 
  mutate(acumulado = cumsum(suma)/cant_trabajadores)

ggplot(a, aes(x=fecha, y=acumulado, group=dosis, color=dosis))  +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(acumulado, digits = 3)), size=4) +
  ylim(0,1)

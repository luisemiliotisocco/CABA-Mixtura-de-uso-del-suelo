library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(rgeos)
library(ggpmisc)

proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica
options(scipen = 999)


densidad  <- read.csv("data/processed/en-proceso/densidad.csv")
entropia  <- read.csv("data/processed/en-proceso/entropia.csv")
granulado  <- read.csv("data/processed/en-proceso/granulado.csv")
diversidad  <- read.csv("data/processed/en-proceso/diversidad.csv")

usos <- densidad %>% 
  left_join(entropia, by="index") %>% 
  left_join(granulado, by="index") %>% 
  left_join(diversidad, by="index")


# PRUEBA CORRELACION

dptos <- read.csv("data/processed/en-proceso/variacion-interanual.csv")%>%
  dplyr::filter(CANTIDAD_T1>4) %>% 
  dplyr::filter(CANTIDAD_T2>4) %>% 
  dplyr::filter(CANTIDAD_T3>4) %>% 
  dplyr::filter(CANTIDAD_T4>4) %>% 
  dplyr::filter(CANTIDAD_T5>4) %>% 
  dplyr::filter(CANTIDAD_T6>4) %>% 
  dplyr::filter(CANTIDAD_T7>4) %>% 
  dplyr::filter(CANTIDAD_T8>4) 

serie_cantidad <- left_join(usos, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT)) %>% 
  select(index, CANTIDAD_T1, CANTIDAD_T2, CANTIDAD_T3, CANTIDAD_T4, CANTIDAD_T5, 
         CANTIDAD_T6, CANTIDAD_T7, CANTIDAD_T8) %>% 
  pivot_longer(cols = CANTIDAD_T1:CANTIDAD_T8, 
               names_to="Category",
               values_to = "Value") %>% 
  pivot_wider(names_from = index,
              values_from = Value) %>%  
  write.csv("data/processed/en-proceso/cantidad-variacion-tiempo.csv")


serie_cantidad <- left_join(usos, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT)) %>% 
  select(index, VT, CANTIDAD_T2, CANTIDAD_T3, CANTIDAD_T4, CANTIDAD_T5, 
         CANTIDAD_T6, CANTIDAD_T7, CANTIDAD_T8) %>% 
  pivot_longer(cols = CANTIDAD_T1:CANTIDAD_T8, 
               names_to="Category",
               values_to = "Value") %>% 
  pivot_wider(names_from = index,
              values_from = Value) %>%  
  write.csv("data/processed/en-proceso/cantidad-variacion-tiempo.csv")


rus_usos <- left_join(usos, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT)) %>% 
  select(index, DENSIDAD, ALT_MAX_PROMEDIO, CANT_USOS, ENT, SUMA_PERIMETRO, CANT_USOS_SECUNDARIOS, TOTAL_USOS_SECUNDARIOS,
         INDICE_SIMPSON, VT, PROMEDIO_T, SD_GENERAL, CV_T)


write.csv(rus_usos, "data/processed/en-proceso/datos-generales.csv")

model <- lm(VT ~ PROMEDIO_PISOS + DENSIDAD + ENT + SUMA_PERIMETRO + CANT_USOS_SECUNDARIOS + INDICE_SIMPSON, data = rus_usos)
model
summary(model)

my.formula <- y ~ x

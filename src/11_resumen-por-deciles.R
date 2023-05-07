library(tidyverse)
library(ggplot2)
library(dplyr)

T1 <- read.csv('data/processed/mercado-inmobiliario/MI_1T.csv', stringsAsFactors = TRUE) %>% 
  mutate(quantile = ntile(USD_M2, 10),
         AREA_R_P=(AREA_R*0.0001)/AREA*100) %>% 
  select(index, AREA_R_P, ENT, INDICE_SIMPSON, PERIMETRO_AREA, DENS_POB, USD_M2, quantile)

T2 <- read.csv('data/processed/mercado-inmobiliario/MI_2P.csv', stringsAsFactors = TRUE) %>% 
  mutate(quantile = ntile(USD_M2, 10),
         AREA_R_P=(AREA_R*0.0001)/AREA*100) %>% 
  select(index, AREA_R_P, ENT, INDICE_SIMPSON, PERIMETRO_AREA, DENS_POB, USD_M2, quantile)

T3 <- read.csv('data/processed/mercado-inmobiliario/MI_3P.csv', stringsAsFactors = TRUE) %>% 
  mutate(quantile = ntile(USD_M2, 10),
         AREA_R_P=(AREA_R*0.0001)/AREA*100) %>% 
  select(index, AREA_R_P, ENT, INDICE_SIMPSON, PERIMETRO_AREA, DENS_POB, USD_M2, quantile)


## Funcion para agrupar

#resumen_quantile <- function(df, column){
#  require(dplyr)
#  column = enquo(arg = column) 
#  resumen <- df %>%
#    group_by(quantile) %>%
#    summarise(average = mean(!!column, na.rm = TRUE),
#              sd= sd(!!column, na.rm = TRUE)) 
#  return(resumen)
#}


T1_resumen <- T1 %>% 
  group_by(quantile) %>% 
  summarise_at(vars(AREA_R_P:DENS_POB), mean, na.rm = TRUE) %>% 
  rename_at(-1, ~ paste0('avg_', .))

T2_resumen <- T2 %>% 
  group_by(quantile) %>% 
  summarise_at(vars(AREA_R_P:DENS_POB), mean, na.rm = TRUE) %>% 
  rename_at(-1, ~ paste0('avg_', .))

T3_resumen <- T3 %>% 
  group_by(quantile) %>% 
  summarise_at(vars(AREA_R_P:DENS_POB), mean, na.rm = TRUE) %>% 
  rename_at(-1, ~ paste0('avg_', .))

# RESIDENCIAL
ggplot()+
  geom_point(data=T1_resumen, aes(x=quantile, y=avg_AREA_R_P), color='#f78a2f')+
  geom_smooth(data=T1_resumen, aes(x=quantile, y=avg_AREA_R_P, color='Prepandémico'), fill=NA)+
  geom_point(data=T2_resumen, aes(x=quantile, y=avg_AREA_R_P), color='#c13262')+
  geom_smooth(data=T2_resumen, aes(x=quantile, y=avg_AREA_R_P, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  geom_point(data=T3_resumen, aes(x=quantile, y=avg_AREA_R_P), color='#370065')+
  geom_smooth(data=T3_resumen, aes(x=quantile, y=avg_AREA_R_P, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  labs(title="Correlación uso residencial - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Decil de precio del suelo (USD/m²)",
       y="Proporción de uso residencial (%)",
       color='Período')+  
  scale_x_continuous(breaks = seq(0, 10, 1))+
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()

# DIVERSIDAD
ggplot()+
  geom_point(data=T1_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON), color='#f78a2f')+
  geom_smooth(data=T1_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON, color='Prepandémico'), fill=NA)+
  geom_point(data=T2_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON), color='#c13262')+
  geom_smooth(data=T2_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  geom_point(data=T3_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON), color='#370065')+
  geom_smooth(data=T3_resumen, aes(x=quantile, y=avg_INDICE_SIMPSON, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  labs(title="Correlación diversidad de usos - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Decil de precio del suelo (USD/m²)",
       y="Índice de diversidad (Simpson)",
       color='Período')+  
  scale_x_continuous(breaks = seq(0, 10, 1))+
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()

# ENTROPIA
ggplot()+
  geom_point(data=T1_resumen, aes(x=quantile, y=avg_ENT), color='#f78a2f')+
  geom_smooth(data=T1_resumen, aes(x=quantile, y=avg_ENT, color='Prepandémico'), fill=NA)+
  geom_point(data=T2_resumen, aes(x=quantile, y=avg_ENT), color='#c13262')+
  geom_smooth(data=T2_resumen, aes(x=quantile, y=avg_ENT, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  geom_point(data=T3_resumen, aes(x=quantile, y=avg_ENT), color='#370065')+
  geom_smooth(data=T3_resumen, aes(x=quantile, y=avg_ENT, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  labs(title="Correlación entropía de usos - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Decil de precio del suelo (USD/m²)",
       y="Índice de entropia",
       color='Período')+  
  scale_x_continuous(breaks = seq(0, 10, 1))+
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()

# DENSIDAD
ggplot()+
  geom_point(data=T1_resumen, aes(x=quantile, y=avg_DENS_POB), color='#f78a2f')+
  geom_smooth(data=T1_resumen, aes(x=quantile, y=avg_DENS_POB, color='Prepandémico'), fill=NA)+
  geom_point(data=T2_resumen, aes(x=quantile, y=avg_DENS_POB), color='#c13262')+
  geom_smooth(data=T2_resumen, aes(x=quantile, y=avg_DENS_POB, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  geom_point(data=T3_resumen, aes(x=quantile, y=avg_DENS_POB), color='#370065')+
  geom_smooth(data=T3_resumen, aes(x=quantile, y=avg_DENS_POB, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  labs(title="Correlación densidad habitacional - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Decil de precio del suelo (USD/m²)",
       y="Densidad habitacional (Hab/ha)",
       color='Período')+  
  scale_x_continuous(breaks = seq(0, 10, 1))+
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()


# INTERACCION
ggplot()+
  geom_point(data=T1_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA), color='#f78a2f')+
  geom_smooth(data=T1_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA, color='Prepandémico'), fill=NA)+
  geom_point(data=T2_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA), color='#c13262')+
  geom_smooth(data=T2_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  geom_point(data=T3_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA), color='#370065')+
  geom_smooth(data=T3_resumen, aes(x=quantile, y=avg_PERIMETRO_AREA, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  labs(title="Correlación granulado de usos - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Decil de precio del suelo (USD/m²)",
       y="Interacción de usos (m perímetro interacción/ha)",
       color='Período')+  
  scale_x_continuous(breaks = seq(0, 10, 1))+
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()



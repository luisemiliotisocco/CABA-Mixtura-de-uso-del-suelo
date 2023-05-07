library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(ggpmisc)
library(ggmap)

proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica
options(scipen = 999)

#___

grid <- st_read("data/processed/unidad-de-analisis/grid.shp") %>% 
  st_transform(proj) 

barrios_CABA <- st_read("data/raw/barrios/barrios_badata.shp") %>% 
  st_transform(proj)

partidos_AMBA <- st_read("data/raw/partidos_amba/amba_partidos_caba_poli.shp") %>% 
  st_transform(proj) %>% 
  st_difference() %>% 
  select(nam) %>% 
  dplyr::filter(nam!="CABA")

partidos_AMBA_solido <- partidos_AMBA %>% 
  st_union()

manzanas <- st_read("data/raw/manzanas/manzanas.geojson") %>% 
  st_transform(proj)

rus <- st_read("data/raw/rus/Usos_del_Suelo_Ciudad Autónoma de Buenos Aires.shp") %>% 
  st_transform(proj)


#___
#Calcular parcelas con mas usos - desvio std
#___


#___
#Grafico con frecuencia de usos secundarios
#___
rus2 <- rus %>% group_by(USO_SUELO_2) %>% mutate(count = n())

ggplot(rus)+
  geom_histogram(aes(TIPO_2), stat="count", fill="orange", alpha=.5, color="black")+
  theme_minimal()


#RUS_grid

rus_grid <- st_join(rus, grid) %>% 
  as.data.frame() %>% 
  select(-geometry)

cantidad_usos_secundarios_grid <- rus_grid %>% 
  group_by(index) %>% 
  summarise(CANT_USOS_SECUNDARIOS=n_distinct(TIPO_2, na.rm=TRUE)) #usos diversos por index (cantidad)

rus_grid_secundarios <- grid %>% 
  left_join(cantidad_usos_secundarios_grid, by="index")


mapa_CABA <- get_stamenmap(bbox = c("left" = -58.5560950647, "bottom" = -34.7154185163, "right" = -58.313297209, "top" = -34.5092633245),
                           maptype = "terrain",
                           zoom=13,
                           source = "google",
                           color="bw")


ggmap(mapa_CABA, darken = c(0.4, "white"))+  
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(rus_grid_secundarios, !is.na(CANT_USOS_SECUNDARIOS)), aes(fill=CANT_USOS_SECUNDARIOS, geometry=geometry), 
          alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Cantidad de usos secundarios",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Cantidad de usos \nsecundarios")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))



#Indice de Simpson
rus_grid_diversidad <- rus_grid %>% 
  group_by(index, TIPO_2) %>% 
  summarise(FRECUENCIA_USO_SECUNDARIO=n()) %>% 
  left_join(cantidad_usos_secundarios_grid, by="index")

rus_grid_simpson <- rus_grid_diversidad %>% 
  group_by(index) %>%
  summarise(TOTAL_USOS_SECUNDARIOS=sum(FRECUENCIA_USO_SECUNDARIO))

rus_grid_simpson_2 <- rus_grid_simpson %>%     
  left_join(rus_grid_diversidad, by="index") %>% 
  mutate(S2=(FRECUENCIA_USO_SECUNDARIO*(FRECUENCIA_USO_SECUNDARIO-1))) %>% 
  group_by(index) %>% 
  summarise(S2_2=sum(S2)) %>% 
  left_join(rus_grid_simpson, by="index") %>% 
  mutate(INDICE_SIMPSON=1-(S2_2)/(TOTAL_USOS_SECUNDARIOS*(TOTAL_USOS_SECUNDARIOS-1)))
  
  

rus_grid_secundarios <- rus_grid_secundarios %>% 
  left_join(rus_grid_simpson_2, by="index")



ggmap(mapa_CABA, darken = c(0.4, "white"))+  
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(rus_grid_secundarios, !is.na(INDICE_SIMPSON)), aes(fill=INDICE_SIMPSON, geometry=geometry), 
          alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Índice de diversidad de Simpson (usos secundarios)",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Índice de diversidad")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))

hist(rus_grid_secundarios$INDICE_SIMPSON)

promedio_uso_sec <- as.numeric(mean(rus_grid_secundarios$INDICE_SIMPSON, na.rm = TRUE))

ggplot(rus_grid_secundarios, y=..count..) + 
  geom_density(aes(INDICE_SIMPSON, y = ..count..), color="darkred", size=1.5, fill="darkred", alpha=.1)+
  geom_vline(xintercept = promedio_uso_sec, color="grey30", size=1, linetype="dashed")+  
  labs(title="Frecuencia del Índice de Diversidad",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x= "Índice de Simpson",
       y="Frecuencia",
       caption="Elaboración propia con datos de BAdata")+  
  theme_minimal(base_line_size = 1)



rus_grid_secundarios %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/diversidad.csv")


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


dptos_uso_secundario <- left_join(rus_grid_secundarios, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT))

ggplot(dptos_uso_secundario, aes(x = INDICE_SIMPSON, y = SD_GENERAL)) +
  geom_point() +
  stat_smooth(method = lm)+
  theme_minimal()

ggplot(dptos_uso_secundario, aes(x = CANT_USOS_SECUNDARIOS, y = SD_GENERAL)) +
  geom_point() +
  stat_smooth(method = lm)+
  theme_minimal()


# Modelo 1
model <- lm(CANT_USOS_SECUNDARIOS ~ SD_GENERAL, data = dptos_uso_secundario)
model
summary(model) 

my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"

ggplot(dptos_uso_secundario, aes(x = CANT_USOS_SECUNDARIOS, y = SD_GENERAL)) +
  geom_point(color="grey30") +
  stat_smooth(method = lm, fill="red", alpha=.1, color="darkred", linetype="dashed")+
  stat_poly_eq(
    formula = my.formula, output.type = "numeric",
    mapping = aes(
      label = 
        sprintf(
          myformat,
          format(stat(coef.ls)[[1]][[1, "Estimate"]], scientific = FALSE, digits =4),
          format(stat(coef.ls)[[1]][[2, "Estimate"]], scientific = FALSE, digits =4),
          formatC(stat(r.squared)))),
    vstep = 0.1)+
  labs(title="Diversidad de usos secundarios - variación del precio promedio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Cantidad absoluta de usos secundarios",
       y="Desvío estándar (USD/m²)")+
  theme_minimal()


# Modelo 2
model2 <- lm(VT ~ CANT_USOS_SECUNDARIOS, data = dptos_uso_secundario)
model2
summary(model2)

ggplot(dptos_uso_secundario, aes(x = CANT_USOS_SECUNDARIOS, y = VT)) +
  geom_point(color="grey30") +
  stat_smooth(method = lm, fill="red", alpha=.1, color="darkred", linetype="dashed")+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), 
               parse = TRUE)+
  labs(title="Diversidad de usos secundarios - variación del precio promedio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Cantidad absoluta de usos secundarios",
       y="Variación total del precio trimestral")+
  theme_minimal()


model3 <- lm(CANT_USOS_SECUNDARIOS ~ PROMEDIO_T, data = dptos_uso_secundario)
model3
summary(model3) 



#Correlación estadisticamente significativa entre INDICE_SIMPSON y CV_T**
#Menos significativa con respecto al SD_GENERAL (0.05)

#TOTAL DE USOS SECUNDATIOS y CANT_USOS_SECUNDARIOS explican estadisticamente significativo el PROMEDIO_T *** INVERSO!!
#TOTAL DE USOS SECUNDATIOS y CANT_USOS_SECUNDARIOS explican estadisticamente significativo el SD_GENERAL ***  INVERSO!!
#TOTAL DE USOS SECUNDATIOS y CANT_USOS_SECUNDARIOS explican estadisticamente significativo el cv_T ***  INVERSO!!

# A mayor cantidad de usos secundarios, la variacion fue menor
# Caida de precios en microcentro (zona con buena cantidad de usos secundarios)



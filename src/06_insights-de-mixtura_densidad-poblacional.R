library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(rgeos)
library(ggpmisc)
library(ggmap)

proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica
options(scipen = 999)

#___

grid <- st_read("data/processed/unidad-de-analisis/grid.shp") %>% 
  st_transform(proj) %>% 
  mutate(AREA=as.numeric(st_area(.))*0.0001) #en hectareas

barrios_CABA <- st_read("data/raw/barrios/barrios_badata.shp") %>% 
  st_transform(proj)

partidos_AMBA <- st_read("data/raw/partidos_amba/amba_partidos_caba_poli.shp") %>% 
  st_transform(proj) %>% 
  st_difference() %>% 
  select(nam) %>% 
  dplyr::filter(nam!="CABA")

partidos_AMBA_solido <- partidos_AMBA %>% 
  st_union()

rus <- st_read("data/raw/rus/Usos_del_Suelo_Ciudad Autónoma de Buenos Aires.shp") %>% 
  st_transform(proj)

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
  st_transform(proj) %>% 
  select(TOT_POB) %>% 
  st_centroid() %>% 
  replace(is.na(.), 0)

altura_parcelaria <- st_read("data/raw/altura-parcelaria/200108_Altura_Parcelaria_Edificacion_2017.shp") %>% 
  st_transform(proj) %>% 
  select(ALT_MAX) %>% 
  st_centroid()

# Densidad poblacional
grid_poblacion <- grid %>%
  st_join(radios) %>% 
  as.data.frame() %>% 
  select(AREA, TOT_POB, index) %>% 
  group_by(index) %>% 
  summarise(SUMA_POBLACION=sum(TOT_POB))

grid_poblacion_grupo <- grid %>% 
  left_join(grid_poblacion, by="index") %>% 
  mutate(DENSIDAD=SUMA_POBLACION/AREA)


mapa_CABA <- get_stamenmap(bbox = c("left" = -58.5560950647, "bottom" = -34.7154185163, "right" = -58.313297209, "top" = -34.5092633245),
                           maptype = "terrain",
                           zoom=13,
                           source = "google",
                           color="bw")


ggmap(mapa_CABA, darken = c(0.4, "white"))+  
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(grid_poblacion_grupo, !is.na(DENSIDAD)), aes(fill=DENSIDAD, geometry=geometry), 
          alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Densidad poblacional",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de INDEC y BAdata",
       fill="Hab/ha")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))

#Densidad edilicia
densidad_edilicia <- grid %>% 
  st_join(altura_parcelaria) %>% 
  as.data.frame() %>% 
  replace(is.na(.), 0) %>% 
  group_by(index) %>% 
  summarise(ALT_MAX_PROMEDIO=mean(ALT_MAX, na.rm=T))


rus_densidad <- grid_poblacion_grupo %>% left_join(densidad_edilicia, by="index")

ggmap(mapa_CABA, darken = c(0.4, "white"))+  
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(rus_densidad, !is.na(ALT_MAX_PROMEDIO)), aes(fill=ALT_MAX_PROMEDIO, geometry=geometry), 
          alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Altura del tejido urbano",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Altura máxima \npromedio (m)")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))

rus_densidad %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/densidad.csv")

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

rus_densidad_regresion <- left_join(rus_densidad, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT))


model <- lm(SD_GENERAL ~ ALT_MAX_PROMEDIO + DENSIDAD, data = rus_densidad_regresion)
model
summary(model)

my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"

ggplot(rus_densidad_regresion, aes(x = DENSIDAD, y = SD_GENERAL)) +
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
  labs(title="Densidad poblacional - variación del precio promedio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de INDEC y Argenprop",
       x="Densidad poblacional (hab/ha)",
       y="Desvío estándar (USD/m²)")+
  theme_minimal()

ggplot(rus_densidad_regresion, aes(x = ALT_MAX_PROMEDIO, y = SD_GENERAL)) +
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
  labs(title="Altura del tejido - variación del precio promedio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Densidad edilicia (altura del tejido en metros)",
       y="Desvío estándar (USD/m²)")+
  theme_minimal()


# La densidad poblacional tiene correlacion significativa con SD_GENERAL INVEERSA!!
# La densidad edilicia tambien, pero menos sifnicativa

# Se valoraron los tejios menos densos 

model2 <- lm(VT ~ ALT_MAX_PROMEDIO + DENSIDAD, data = rus_densidad_regresion)
model2
summary(model2)

ggplot(rus_densidad_regresion, aes(x = DENSIDAD, y = VT)) +
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
  labs(title="Densidad poblacional - recuperación del precio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de INDEC y Argenprop",
       x="Densidad poblacional (hab/ha)",
       y="Variación total del precio trimestral")+
  theme_minimal()

ggplot(rus_densidad_regresion, aes(x = ALT_MAX_PROMEDIO, y = VT)) +
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
  labs(title="Altura del tejido - recuperación del precio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Densidad edilicia (altura del tejido en metros)",
       y="Variación total del precio trimestral")+
  theme_minimal()



#--

promedio_alt <- mean(rus_densidad_regresion$ALT_MAX_PROMEDIO)
promedio_dens <- mean(rus_densidad_regresion$DENSIDAD)

ggplot(rus_densidad_regresion, y=..count..) + 
  geom_density(aes(ALT_MAX_PROMEDIO, y = ..count..), color="darkred", size=1.5, fill="darkred", alpha=.1)+
  geom_vline(xintercept = promedio_alt, color="grey30", size=1, linetype="dashed")+  
  labs(title="Frecuencia de altura del tejido edilicio",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x= "Altura del tejido (m)",
       y="Frecuencia",
       caption="Elaboración propia con datos de BAdata")+  
  theme_minimal(base_line_size = 1)

ggplot(rus_densidad_regresion, y=..count..) + 
  geom_density(aes(DENSIDAD, y = ..count..), color="darkred", size=1.5, fill="darkred", alpha=.1)+
  #geom_histogram(aes(DENSIDAD), color="darkred", size=1.5, fill="darkred", alpha=.1)+
  geom_vline(xintercept = promedio_dens, color="grey30", size=1, linetype="dashed")+  
  labs(title="Frecuencia de densidad poblacional",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x= "Densidad poblacional (Hab/ha)",
       y="Frecuencia",
       caption="Elaboración propia con datos de INDEC")+  
  theme_minimal(base_line_size = 1)

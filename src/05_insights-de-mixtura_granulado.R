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

rus_recategorizado <- read.csv2("data/raw/rus/RUS_USOS-RECATEGORIZADOS.csv", encoding = "UTF-8") %>% 
  rename(TIPO_2=X.U.FEFF.TIPO_2)

rus_parcela <- st_read("data/raw/rus/Usos del Suelo. Ciudad Autónoma de Buenos Aires 2016-2017_parcelas.shp", options = "ENCODING=UTF8") %>% 
  st_transform(proj)%>% 
  select(TIPO2_16, SMP) %>% 
  rename(TIPO_2=TIPO2_16)

rus_parcela<- rus_parcela %>% left_join(rus_recategorizado, by="TIPO_2") 
rus_parcela$USO_SUELO_2 <- rus_parcela$USO_SUELO_2 %>% replace_na("RecreativoCultural")
#  

granulado <- rus_parcela %>% 
  group_by(USO_SUELO_2) %>% 
  summarise()

#st_write(granulado, "data/processed/granulado/granulado.shp", delete_dsn = TRUE)

grid_gradunado <- grid %>% 
  st_intersection(granulado) %>% 
  mutate(PERIMETRO = st_length(.))

grid_50 <- grid %>% 
  dplyr::filter(index==96) %>% 
  st_intersection(rus_parcela)

ggplot()+
  geom_sf(data=grid_gradunado %>% dplyr::filter(index==96), aes(fill=USO_SUELO_2), show.legend = FALSE) +
  geom_sf(data=grid_50, fill=NA, color="grey40") +
  scale_fill_viridis_d(option = "magma") + 
  labs(title="Interacción de usos - granulado del tejido",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Usos principales por parcela")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))



grid_gradunado_df <- grid_gradunado %>% 
  as.data.frame() %>% 
  select (index, PERIMETRO) %>% 
  group_by(index) %>% 
  summarise(SUMA_PERIMETRO=as.numeric(sum(PERIMETRO)))

grid_perimetro <- grid %>% 
  left_join(grid_gradunado_df, by="index")


mapa_CABA <- get_stamenmap(bbox = c("left" = -58.5560950647, "bottom" = -34.7154185163, "right" = -58.313297209, "top" = -34.5092633245),
                           maptype = "terrain",
                           zoom=13,
                           source = "google",
                           color="bw")


ggmap(mapa_CABA, darken = c(0.4, "white"))+  
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(grid_perimetro, !is.na(SUMA_PERIMETRO)), aes(fill=SUMA_PERIMETRO, geometry=geometry), 
          alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Granulado del tejido de usos",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Perímetro de \ninteracción (m)")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))

hist(grid_perimetro$SUMA_PERIMETRO)

grid_perimetro %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/granulado.csv")

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

granulado_perimetro <- left_join(grid_perimetro, dptos, by="index") %>% 
  dplyr::filter(!is.na(VT))

# Modelo 1
model <- lm(SD_GENERAL ~ SUMA_PERIMETRO, data = granulado_perimetro)
model
summary(model) 

my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"

ggplot(granulado_perimetro, aes(x = SUMA_PERIMETRO, y = SD_GENERAL)) +
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
  labs(title="Granulado del tejido de usos - variaciación del precio promedio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Suma perímetro interacción de usos (m)",
       y="Desvío estándar (USD/m²)")+
  theme_minimal()


# Modelo 2
model2 <- lm(VT ~ SUMA_PERIMETRO, data = granulado_perimetro)
model2
summary(model2) 


ggplot(granulado_perimetro, aes(x = SUMA_PERIMETRO, y = VT)) +
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
  labs(title="Granulado del tejido de usos",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Suma perímetro interacción de usos (m)",
       y="Variación total del precio trimestral")+
  theme_minimal()
# GRANULADO TIENE EFECTO SIGNIFICATIVO EN EL PRECIO PROMEDIO ANUAL ***
# GRANULADO TIENE EFECTO SIGNIFICATIVO EN EL SD_GENERAL ***





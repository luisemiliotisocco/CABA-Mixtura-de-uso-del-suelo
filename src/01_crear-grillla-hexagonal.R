library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(ggmap)

proj <- 4326
sf::sf_use_s2(TRUE) 

#___

barrios_CABA <- st_read("data/raw/barrios/barrios_badata.shp") %>% 
  st_transform(proj)

partidos_AMBA <- st_read("data/raw/partidos_amba/amba_partidos_caba_poli.shp") %>% 
  st_transform(proj) %>% 
  st_difference() %>% 
  select(nam) %>% 
  dplyr::filter(nam!="CABA")

partidos_AMBA_solido <- partidos_AMBA %>% 
  st_union()

bbox_CABA_limite <- getbb("Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")
bbox_CABA_limite <- bbox_CABA_limite$multipolygon %>% 
  st_transform(proj)

grid_size=.015#0.175

grid <- st_make_grid(bbox_CABA_limite, cellsize = grid_size, what = "polygons", square = FALSE) #creamos la malla hexagonal

grid <- grid %>% 
  st_as_sf(index = 1:length(lengths(grid)), grid) %>%  #le agregamos un índice
  st_transform(crs = 4326) #mismo crs

#___

hex <- st_intersection (grid, bbox_CABA_limite) %>% 
  st_difference()

#Veamos cómo se ve: 

mapa_CABA <- get_stamenmap(bbox = c("left" = -58.5560950647, "bottom" = -34.7154185163, "right" = -58.313297209, "top" = -34.5092633245),
                           maptype = "terrain",
                           zoom=13,
                           source = "google",
                           color="bw")


ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data = barrios_CABA, fill=NA, size=.8, inherit.aes = FALSE) + 
  geom_sf(data=hex, aes(geometry=grid), fill=NA, color="#B43757", size=.8, linetype="dashed",inherit.aes = FALSE)+
  labs(title="Gilla hexagonal - Unidad de análisis virtual",
       subtitle="Ciudad Autónoma de Buenos Aires",
       caption = "Fuente: BA Data")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15))

#___
#Guardamos

st_write(hex, "data/processed/unidad-de-analisis/grid.shp", delete_dsn = TRUE)

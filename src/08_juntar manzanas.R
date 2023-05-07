library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(ggmap)
library(lwgeom)


proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

#___

manzanas <- st_read("data/raw/manzanas/manzanas.shp") %>% 
  st_transform(proj) %>% 
  rename(index=OBJECTID) %>% 
  select(index)

comuna_1 <- read.csv("data/processed/data-por-comunas/comuna-1.csv") %>% 
  select(-geometry)
comuna_2 <- read.csv("data/processed/data-por-comunas/comuna-2.csv") %>% 
  select(-geometry)
comuna_3 <- read.csv("data/processed/data-por-comunas/comuna-3.csv") %>% 
  select(-geometry)
comuna_4 <- read.csv("data/processed/data-por-comunas/comuna-4.csv") %>% 
  select(-geometry)
comuna_5 <- read.csv("data/processed/data-por-comunas/comuna-5.csv") %>% 
  select(-geometry)
comuna_6 <- read.csv("data/processed/data-por-comunas/comuna-6.csv") %>% 
  select(-geometry)
comuna_7 <- read.csv("data/processed/data-por-comunas/comuna-7.csv") %>% 
select(-geometry)
comuna_8 <- read.csv("data/processed/data-por-comunas/comuna-8.csv")
comuna_9 <- read.csv("data/processed/data-por-comunas/comuna-9.csv") %>% 
  select(-geometry.y)
comuna_10 <- read.csv("data/processed/data-por-comunas/comuna-10.csv")
comuna_11a <- read.csv("data/processed/data-por-comunas/comuna-11a.csv") %>% 
  select(-geometry)
comuna_11b <- read.csv("data/processed/data-por-comunas/comuna-11b.csv") %>% 
  select(-geometry)
comuna_12a <- read.csv("data/processed/data-por-comunas/comuna-12a.csv") %>% 
  select(-geometry)
comuna_12b <- read.csv("data/processed/data-por-comunas/comuna-12b.csv") %>% 
  select(-geometry)
comuna_13 <- read.csv("data/processed/data-por-comunas/comuna-13.csv") %>% 
  select(-geometry)
comuna_14 <- read.csv("data/processed/data-por-comunas/comuna-14.csv") %>% 
  select(-geometry)
comuna_15 <- read.csv("data/processed/data-por-comunas/comuna-15.csv") %>% 
  select(-geometry)

join <- bind_rows(comuna_1, comuna_2, comuna_3, comuna_4, comuna_5, comuna_6,
                  comuna_7, comuna_8, comuna_9, comuna_10, comuna_11a, comuna_11b,
                  comuna_12a, comuna_12b, comuna_13, comuna_14, comuna_15) 

write.csv(join, 'data/processed/manzanas/manzanas-con-usos.csv')

manzanas_join <- manzanas %>% 
  left_join(join, by='index') %>% 
  mutate(PERIMETRO_HA=SUMA_PERIMETRO/AREA)
  
ggplot()+
  geom_sf(data=manzanas_join, aes(fill=INDICE_SIMPSON), color=NA)+
  scale_fill_viridis_c(option = 'inferno', direction=1)+
  theme_void()

write_sf(manzanas_join, 'data/processed/manzanas/manzanas-con-usos.shp', delete_dsn = TRUE)


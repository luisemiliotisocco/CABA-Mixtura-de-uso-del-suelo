library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(ggmap)
library(lwgeom)


proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

#___

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
  st_transform(proj) %>% 
  select(TOT_POB) %>% 
  st_centroid()

manzanas <- st_read("data/raw/manzanas/manzanas.shp") %>% 
  st_transform(proj) %>% 
  rename(index=OBJECTID) %>% 
  select(index, SM, BARRIOS, COMUNAS) #%>% 
  dplyr::filter(index>=7000)

# Para agregar index
#manzanas$index <- 1:nrow(manzanas)

# Me guardo manzanas procesadas
#manzanas %>% st_collection_extract("POLYGON") %>% 
#st_write("data/raw/manzanas/manzanas.shp", delete_dsn = TRUE)

#rus_parcela <- st_read("data/raw/rus/Usos del Suelo. Ciudad Autónoma de Buenos Aires 2016-2017_parcelas.shp", options = "ENCODING=UTF8") %>% 
#  st_transform(proj) %>% 
#  mutate(AREA=as.numeric(st_area(geometry))) %>% 
#  select(SMP, TIPO1_16, TIPO2_16, AREA) %>% 
#  rename(TIPO_1=TIPO1_16,
#         TIPO_2=TIPO2_16)

#st_write(rus_parcela, "data/raw/rus/RUS_parcelas.shp", delete_dsn = TRUE)

rus_parcela <- st_read("data/raw/rus/RUS_parcelas.shp", options = "ENCODING=UTF8") %>% 
  st_transform(proj)

rus_recategorizado <- read.csv2("data/raw/rus/RUS_USOS-RECATEGORIZADOS.csv", encoding = "UTF-8") %>%
  rename(TIPO_2=X.U.FEFF.TIPO_2, 
         USO_SUELO_PPAL=USO_SUELO_2, 
         USO_SUELO_SEC=TIPO_2_2) %>% 
  select(TIPO_2, USO_SUELO_PPAL, USO_SUELO_SEC)

rus <- rus_recategorizado %>% 
  left_join(rus_parcela, by="TIPO_2") %>%
  dplyr::filter(!USO_SUELO_PPAL %in% c('Infraestructura', 'Otro')) %>% #Estos usos se omiten p/calculo
  st_as_sf(sf_column_name = "geometry")

# rus points
rus_point <- rus %>%   
  st_centroid

## COMUNA 

comunas_n <- 13

isocronas <- st_read("data/processed/isocronas-15-minutos/isocronas_comuna_13.shp", options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  mutate(COMUNA=comunas_n) #%>% 
#  dplyr::filter(index>=7000)
#  rename(index=id) #

#st_write(isocronas, 'data/processed/isocronas-15-minutos/isocronas_comuna_15.shp', delete_dsn = TRUE)


# ALTERNATIVAMETE A ISOCRONAS, SEPUEDE HACER UN BUFFER CALCULANDO LA DISTANCIA CAMINADO A VELOCIDAD PROMEDIO
#buffer <- manzanas %>% 
#  dplyr::filter(COMUNAS==comunas_n) %>%
#  st_buffer(500)

#___



# ENTROPIA

isocronas_join <- st_intersection(rus_point, isocronas)
  
isocronas_join_df <- isocronas_join %>%
  as.data.frame() %>% 
  select(-geometry) %>%
  group_by(index) %>% 
  summarise(CANT_USO_PPAL=n_distinct(USO_SUELO_PPAL), #conteo de usos principales distintos
            CANT_USO_SEC=n_distinct(USO_SUELO_SEC), #conteo de usos principales distintos 
            SUPERFICIE_USO=sum(AREA),
            Q_C=sum(USO_SUELO_PPAL == 'Comercial', na.rm=TRUE), 
            #Q_I=sum(USO_SUELO_PPAL == 'Infraestructura'),
            #Q_O=sum(USO_SUELO_PPAL == 'Otro'),
            Q_P=sum(USO_SUELO_PPAL == 'Productivo'), 
            Q_EP=sum(USO_SUELO_PPAL == 'Publico'), 
            Q_RC=sum(USO_SUELO_PPAL == 'RecreativoCultural'), 
            Q_R=sum(USO_SUELO_PPAL == 'Residencial'),
            Q_S=sum(USO_SUELO_PPAL == 'Servicios'),
            AREA_C=sum(AREA[USO_SUELO_PPAL=="Comercial"]),
            #AREA_I=sum(AREA[USO_SUELO_PPAL=="Infraestructura"]),
            #AREA_O=sum(AREA[USO_SUELO_PPAL=="Otro"]),
            AREA_P=sum(AREA[USO_SUELO_PPAL=="Productivo"]),
            AREA_EP=sum(AREA[USO_SUELO_PPAL=="Publico"]),
            AREA_RC=sum(AREA[USO_SUELO_PPAL=="RecreativoCultural"]),
            AREA_R=sum(AREA[USO_SUELO_PPAL=="Residencial"]),
            AREA_S=sum(AREA[USO_SUELO_PPAL=="Servicios"]))
  
manzanas_1 <- manzanas %>% 
  dplyr::filter(COMUNAS==comunas_n) %>% 
  left_join(isocronas_join_df, by='index')
  
manzanas_1_2 <- manzanas_1 %>% 
  rowwise() %>% 
  mutate_at(vars("AREA_C":"AREA_S"),list(PROP=~./SUPERFICIE_USO)) %>%
  mutate_at(vars("AREA_C_PROP":"AREA_S_PROP"),list(LOG=~.*log(.))) %>%
  replace(is.na(.), 0) %>% 
  mutate(ENT=-(sum(c_across("AREA_C_PROP_LOG":"AREA_S_PROP_LOG")))/log(CANT_USO_PPAL)) %>% 
  st_as_sf(sf_column_name = "geometry")

ggplot()+
  geom_sf(data=manzanas_1_2, aes(fill=ENT))+
  scale_fill_viridis_c(option = 'magma', direction = -1)+
  theme_void()


# DIVERSIDAD

isocronas_join_df_2 <- isocronas_join %>% 
  as.data.frame() %>% 
  select(-geometry) %>%
  group_by(index, USO_SUELO_SEC) %>% 
  summarise(CANT_USO_SEC_I=n()) #cantidad INDIVIDUAL de cada uso secundario segun aparece

isocronas_join_df_3 <- isocronas_join_df_2 %>% 
  group_by(index) %>% 
  summarise(CANT_USO_SEC_T=sum(CANT_USO_SEC_I, na.rm = TRUE)) #suma TOTAL de usos secundarios que aparecen

indice_diversidad <- isocronas_join_df_2 %>% 
  left_join(isocronas_join_df_3, by='index') %>% 
  mutate(S2=(CANT_USO_SEC_I*(CANT_USO_SEC_I-1))) %>% #caculo numerador
  group_by(index) %>% 
  summarise(S2_2=sum(S2, na.rm = TRUE)) %>% # calculo del numerador del indice
  left_join(isocronas_join_df_3, by='index') %>% 
  mutate(INDICE_SIMPSON=1-(S2_2)/(CANT_USO_SEC_T*(CANT_USO_SEC_T-1))) #divido por denominador

manzanas_1_3 <- manzanas_1_2 %>% 
  left_join(indice_diversidad, by='index') 

ggplot()+
  geom_sf(data=manzanas_1_3, aes(fill=INDICE_SIMPSON))+
  scale_fill_viridis_c(option = 'magma', direction = -1)+
  theme_void()

#####
manzanas_1_3 %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  write_csv("data/processed/data-por-comunas/PARCIAL.csv")

carga <- read.csv("data/processed/data-por-comunas/PARCIAL2.csv")

manzanas_1_3 <- manzanas %>% 
  dplyr::filter(COMUNAS==8) %>% 
  select(index) %>% 
  left_join(carga)
####

# GRANULADO DE USOS (INTERACCION)

#granulado <- rus %>% 
#  group_by(USO_SUELO_PPAL) %>% 
#  summarise()

#st_write(granulado, 'data/processed/granulado/granulado.shp', delete_dsn = TRUE)

granulado <- st_read("data/processed/granulado/granulado.shp") %>% 
  st_transform(proj)

buffer_gradunado <- isocronas %>% 
  select(index) %>% 
  st_intersection(granulado) %>% 
  mutate(PERIMETRO = as.numeric(st_length(.)))

buffer_granulado_2 <- buffer_gradunado %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(SUMA_PERIMETRO=as.numeric(sum(PERIMETRO, na.rm = TRUE)))


manzanas_1_4 <- manzanas_1_3 %>% 
  left_join(buffer_granulado_2, by='index')

ggplot()+
  geom_sf(data=manzanas_1_4, aes(fill=SUMA_PERIMETRO))+
  scale_fill_viridis_c(option = 'magma', direction = -1)+
  theme_void()



# DENSIDAD

area_buffer <- isocronas %>% 
  mutate(AREA=as.numeric(st_area(.)*0.0001)) %>% 
  as.data.frame() %>% 
  select(index, AREA)

poblacion <- isocronas %>% 
  st_intersection(radios) %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  group_by(index) %>% 
  summarise(POB_BUFFER=sum(TOT_POB, na.rm = TRUE)) %>% 
  left_join(area_buffer, by='index') %>% 
  mutate(DENS_POB=POB_BUFFER/AREA) %>% 
  replace(is.na(.), 0)


manzanas_1_5 <- manzanas_1_4 %>% 
  left_join(poblacion, by='index')

ggplot()+
  geom_sf(data=manzanas_1_5, aes(fill=DENS_POB))+
  scale_fill_viridis_c(option = 'magma', direction = -1)+
  theme_void()

manzanas_1_5 %>% 
  as.data.frame() %>% 
  select(-geometry, -AREA_C_PROP_LOG:AREA_S_PROP_LOG) %>% 
  mutate(ENT=as.numeric(ENT)) %>% 
  replace(is.na(.), 0) %>% 
  write_csv("data/processed/data-por-comunas/comuna-13.csv")

#m12a <- read.csv("data/processed/data-por-comunas/comuna-12a_sin-I-O.csv")
#m12b <- read.csv("data/processed/data-por-comunas/comuna-12b_sin-I-O.csv")

#manzanas_1_5_2 <- bind_rows(m12a, m12b)

library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(caret)
library(ggpmisc)
library(ggmap)

proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica
options(scipen = 999)

#___

grid <- st_read("data/processed/unidad-de-analisis/grid.shp") %>% 
  st_transform(proj) %>% 
  st_difference()

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

rus_parcela <- st_read("data/raw/rus/Usos del Suelo. Ciudad Autónoma de Buenos Aires 2016-2017_parcelas.shp", options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  mutate(AREA=as.numeric(st_area(geometry)))

rus <- rus_parcela %>%   
  st_centroid %>% 
  select(SMP, TIPO1_16, TIPO2_16, AREA) %>% 
  rename(TIPO_1=TIPO1_16,
         TIPO_2=TIPO2_16)

rus_recategorizado <- read.csv2("data/raw/rus/RUS_USOS-RECATEGORIZADOS.csv", encoding = "UTF-8") %>% 
  rename(TIPO_2=X.U.FEFF.TIPO_2)

rus <- rus %>% left_join(rus_recategorizado, by="TIPO_2") 
rus$USO_SUELO_2 <- rus$USO_SUELO_2 %>% replace_na("RecreativoCultural")


#Distribucion de usos
rus2 <- rus %>% group_by(USO_SUELO_2) %>% mutate(count = n())

ggplot()+
  geom_bar(data=rus2, aes(y=reorder(USO_SUELO_2, -count)), fill="darkred", alpha=.5, color="black")+
  labs(title="Usos principales por parcelas",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       y="",
       x="Frecuencia")+ 
  theme_minimal()

#write_sf(rus, "data/processed/en-proceso/rus_recategorizado.shp", delete_layer = TRUE)


# Cantidad de usos por parcela

rus_cantidad_parcela <- rus %>% 
  as.data.frame() %>% 
  group_by(SMP) %>% 
  summarise(CANT_USO_P=n(),
            CANT_USO_PPAL_P=n_distinct(USO_SUELO_2))

rus_cantidad_parcela2 <- rus_cantidad_parcela %>% group_by(CANT_USO_PPAL_P) %>% mutate(count = n())


ggplot()+
  geom_bar(data=rus_cantidad_parcela2, aes(y=reorder(CANT_USO_PPAL_P, -count)), fill="darkred", alpha=.5, color="black")+
  labs(title="Cantidad de usos principales por parcelas",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       y="",
       x="Frecuencia")+ 
  theme_minimal()


rus_parcela <- rus_parcela %>% 
  left_join(rus_cantidad_parcela, by="SMP")


#RUS_grid

rus_grid <- st_join(rus, grid) %>% 
  as.data.frame() %>% 
  select(-geometry)

cantidad_usos_grid <- rus_grid %>% 
  group_by(index) %>% 
  summarise(CANT_USOS=n_distinct(USO_SUELO_2, na.rm=TRUE))

rus_grid <- rus_grid %>% 
  left_join(cantidad_usos_grid, by="index")

rus_grid_uso <- rus_grid %>% 
  group_by(index, USO_SUELO_2) %>% 
  summarise(SUP_USO=sum(as.numeric(AREA),na.rm=TRUE))%>%
  left_join(cantidad_usos_grid, by="index") %>% 
  pivot_wider(names_from = USO_SUELO_2, values_from = SUP_USO) %>% 
  rowwise() %>% 
  mutate(AREA_GRID = sum(c_across("Administrativo":"Otro"), na.rm = TRUE)) %>% 
  left_join(grid, by="index") %>%
  replace(is.na(.), 0) %>% 
  mutate_at(vars("Administrativo":"Otro"),list(PROP=~./AREA_GRID)) %>%
  mutate_at(vars("Administrativo_PROP":"Otro_PROP"),list(LOG=~.*log(.))) %>%
  replace(is.na(.), 0) %>% 
  mutate(ENT=-(sum(c_across("Administrativo_PROP_LOG":"Otro_PROP_LOG")))/log(CANT_USOS))
  

ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=rus_grid_uso %>% dplyr::filter(Servicios_PROP<=1), aes(fill=Servicios_PROP, geometry=geometry), alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7, limits=c(0, 1)) +
  labs(title="Proporción de usos principales por grid",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Uso Servicios")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))


ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(rus_grid_uso, !is.na(ENT)), aes(fill=ENT, geometry=geometry), alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7, limits=c(0, 1)) +
  labs(title="Índice de entropía de usos",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de BAdata",
       fill="Entropía")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))

# entropia
promedio_uso_ent <- as.numeric(mean(rus_grid_uso$ENT, na.rm = TRUE))

ggplot(rus_grid_uso, y=..count..) + 
  geom_density(aes(ENT, y = ..count..), color="darkred", size=1.5, fill="darkred", alpha=.1)+
  geom_vline(xintercept = promedio_uso_ent, color="grey30", size=1, linetype="dashed")+  
  labs(title="Frecuencia del Índice de Mixtura de Usos",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x= "Índice de Entropía",
       y="Frecuencia",
       caption="Elaboración propia con datos de BAdata")+  
  xlim(c(0,1))+
  theme_minimal(base_line_size = 1)



rus_grid_uso %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/entropia.csv")

# PRUEBA CORRELACION

dptos <- read.csv("data/processed/en-proceso/variacion-interanual.csv") %>%
  dplyr::filter(CANTIDAD_T1>4) %>% 
  dplyr::filter(CANTIDAD_T2>4) %>% 
  dplyr::filter(CANTIDAD_T3>4) %>% 
  dplyr::filter(CANTIDAD_T4>4) %>% 
  dplyr::filter(CANTIDAD_T5>4) %>% 
  dplyr::filter(CANTIDAD_T6>4) %>% 
  dplyr::filter(CANTIDAD_T7>4) %>% 
  dplyr::filter(CANTIDAD_T8>4) 


dptos_uso <- left_join(rus_grid_uso, dptos, by="index") %>% 
  dplyr::filter(!is.na(CV_T))


ggplot(dptos_uso, aes(x = ENT, y = VT)) +
  geom_point() +
  stat_smooth(method = lm)+
  theme_minimal()

ggplot(dptos_uso, aes(x = ENT, y = PROMEDIO_T)) +
  geom_point() +
  stat_smooth(method = lm)+
  theme_minimal()


# Modelo 1: Precio vs proporcion de usos
model1 <- lm(PROMEDIO_T ~ Administrativo_PROP_LOG + Comercial_PROP_LOG + Productivo_PROP_LOG + RecreativoCultural_PROP_LOG + Residencial_PROP_LOG+
              Servicios_PROP_LOG, data = dptos_uso)
model1
summary(model1) #Hay correlacion con log de recreativo residencial y servicios


model2 <- lm(PROMEDIO_T ~ ENT, data = dptos_uso)
model2
summary(model2)  #No hay correlacion

my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"


ggplot(dptos_uso, aes(x = ENT, y = PROMEDIO_T)) +
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
  labs(title="Correlación entropía de usos - promedio trimestral",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Índice de entropía",
       y="Precio promedio propiedades en venta (USD/m²)")+
  theme_minimal()


model3 <- lm(VT ~ ENT, data = dptos_uso)
model3
summary(model3)  #Baja coreelación


ggplot(dptos_uso, aes(x = ENT, y = VT)) +
  geom_point(color="grey30") +
  stat_smooth(method = lm, fill="red", alpha=.1, color="darkred", linetype="dashed")+
  stat_poly_eq(
    formula = my.formula, output.type = "numeric",
    eq.with.lhs = "italic(hat(y))~`=`~",
    mapping = aes(
      label = 
        sprintf(
          myformat,
          format(stat(coef.ls)[[1]][[1, "Estimate"]], scientific = FALSE, digits =4),
          format(stat(coef.ls)[[1]][[2, "Estimate"]], scientific = FALSE, digits =4),
          formatC(stat(r.squared)))),
    vstep = 0.1)+
  labs(title="Correlación entropía de usos - promedio trimestral",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Índice de entropía",
       y="Variación total del precio trimestral")+
  theme_minimal()


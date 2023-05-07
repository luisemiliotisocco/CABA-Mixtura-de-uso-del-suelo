library(sf)
library(tidyverse)
library(ggplot2)
library(osmdata)
library(lubridate)
library(stringr)    
library(matrixStats)
#install.packages("ggpmisc")
library(ggpmisc)
library(ggmap)


proj <- 4326

#___

grid <- st_read("data/processed/unidad-de-analisis/grid.shp") %>% 
  st_transform(proj) %>% 
  st_difference()

barrios_CABA <- st_read("data/raw/barrios/barrios_badata.shp") %>% 
  st_transform(proj) %>% 
  select(BARRIO)

partidos_AMBA <- st_read("data/raw/partidos_amba/amba_partidos_caba_poli.shp") %>% 
  st_transform(proj) %>% 
  st_difference() %>% 
  select(nam) %>% 
  dplyr::filter(nam!="CABA")

partidos_AMBA_solido <- partidos_AMBA %>% 
  st_union()

manzanas <- st_read("data/raw/manzanas/manzanas.geojson") %>% 
  st_transform(proj)

#1 semestre
dptos_1_semestre <- st_read("data/raw/propiedades/2020/Venta/Anual/210129_Deptos_Vta_Anual_2020.shp", 
                            stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(FechaPubli, PrecioUsd, UsdPorM2, ANTIG, AMBIENTES) %>% 
  rename(FECHA=FechaPubli, 
         PRECIO_USD=PrecioUsd, 
         USD_M2=UsdPorM2) %>% 
  mutate(ANTIG=as.double(ANTIG)) %>% 
  mutate(FECHA=as.Date(FECHA, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
  arrange(FECHA) %>% 
  mutate(AÑO=year(FECHA),
         MES=month(FECHA),
         TRIMESTRE=case_when(MES %in%c(1,2,3) ~ 1,
                             MES %in%c(4,5,6) ~ 2)) %>% 
  dplyr::filter(AÑO==2020) %>% 
  st_join(barrios_CABA) 


#3 trimestre
dptos_3_trimestre <- st_read("data/raw/propiedades/2020/Venta/3-Trimestre/200110_deptos_vta_3trimestre_v3.shp", 
                             stringsAsFactors = TRUE) %>% 
  st_transform(proj) %>% 
  select(FECHA, BARRIOS, COMUNA, DOLARES, U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(PRECIO_USD=DOLARES,
         USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(FECHA=as.Date(FECHA, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
  mutate(AÑO=year(FECHA),
         MES=month(FECHA),
         TRIMESTRE=3) %>% 
  st_join(barrios_CABA) 

#4 trimestre
dptos_4_trimestre <- st_read("data/raw/propiedades/2020/Venta/4-Trimestre/deptos_vta_4°trimestre_2020_vFinal.shp", 
                             stringsAsFactors = TRUE) %>% 
  st_transform(proj) %>% 
  select(FECHA, BARRIOS, COMUNA, DOLARES, U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(PRECIO_USD=DOLARES,
         USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(FECHA=as.Date(FECHA, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) %>% 
  mutate(AÑO=year(FECHA),
         MES=month(FECHA),
         TRIMESTRE=4) %>% 
  st_join(barrios_CABA) 


dpto_anual <- dptos_1_semestre %>% 
  dplyr::bind_rows(dptos_3_trimestre) %>% 
  dplyr::bind_rows(dptos_4_trimestre)
  


# TRIMESTRE 1
barrio_trimestre_1 <- dptos_1_semestre %>% 
  as.data.frame() %>% 
  dplyr::filter(TRIMESTRE==1) %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T1=mean(USD_M2),
            SD_BARRIO_T1=sd(USD_M2),
            CANTIDAD_BARRIO_T1=n())

# TRIMESTRE 2
barrio_trimestre_2 <- dptos_1_semestre %>% 
  as.data.frame() %>% 
  dplyr::filter(TRIMESTRE==2) %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T2=mean(USD_M2),
            SD_BARRIO_T2=sd(USD_M2),
            CANTIDAD_BARRIO_T2=n())

# TRIMESTRE 3
barrio_trimestre_3 <- dptos_3_trimestre %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T3=mean(USD_M2),
            SD_BARRIO_T3=sd(USD_M2),
            CANTIDAD_BARRIO_T3=n())

# TRIMESTRE 4
barrio_trimestre_4 <- dptos_4_trimestre %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T4=mean(USD_M2),
            SD_BARRIO_T4=sd(USD_M2),
            CANTIDAD_BARRIO_T4=n())

barrio_anual <-  barrio_trimestre_1 %>% 
  inner_join (barrio_trimestre_2, by="BARRIO") %>% 
  inner_join(barrio_trimestre_3, by="BARRIO") %>% 
  inner_join(barrio_trimestre_4, by="BARRIO") %>% 
  mutate(V1=PROMEDIO_BARRIO_T2/PROMEDIO_BARRIO_T1-1, #variaciones trimestrales
         V2=PROMEDIO_BARRIO_T3/PROMEDIO_BARRIO_T2-1,
         V3=PROMEDIO_BARRIO_T4/PROMEDIO_BARRIO_T3-1) #variacion anual



#2021
#TRIMESTRE 1-2021
dptos_1_trimestre_2021 <- st_read("data/raw/propiedades/2021/Venta/1-Trimestre/deptos_vta_1°trimestre_2021_v2.shp", 
                            stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=5) %>% 
  st_join(barrios_CABA) 

barrio_trimestre_5 <- dptos_1_trimestre_2021 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T5=mean(USD_M2),
            SD_BARRIO_T5=sd(USD_M2),
            CANTIDAD_BARRIO_T5=n())

#TRIMESTRE 2-2021
dptos_2_trimestre_2021 <- st_read("data/raw/propiedades/2021/Venta/2-Trimestre/deptoventa_2trimestre_2021.shp", 
                                  stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=6) %>% 
  st_join(barrios_CABA) 

barrio_trimestre_6 <- dptos_2_trimestre_2021 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T6=mean(USD_M2),
            SD_BARRIO_T6=sd(USD_M2),
            CANTIDAD_BARRIO_T6=n())

#TRIMESTRE 3-2021
dptos_3_trimestre_2021 <- st_read("data/raw/propiedades/2021/Venta/3-Trimestre/Deptos_Vta_3°Trimestre_2021.shp", 
                                  stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=7) %>% 
  st_join(barrios_CABA) 

barrio_trimestre_7 <- dptos_3_trimestre_2021 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T7=mean(USD_M2),
            SD_BARRIO_T7=sd(USD_M2),
            CANTIDAD_BARRIO_T7=n())

#TRIMESTRE 4-2021
dptos_4_trimestre_2021 <- st_read("data/raw/propiedades/2021/Venta/4-Trimestre/deptos_vta_4°trimestre_2021.shp", 
                                  stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=8) %>% 
  st_join(barrios_CABA) 

barrio_trimestre_8 <- dptos_4_trimestre_2021 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  summarise(PROMEDIO_BARRIO_T8=mean(USD_M2),
            SD_BARRIO_T8=sd(USD_M2),
            CANTIDAD_BARRIO_T8=n())


dpto_inter_anual <- dpto_anual %>% 
  select(USD_M2, ANTIG, AMBIENTES, TRIMESTRE, BARRIO) %>% 
  dplyr::bind_rows(dptos_1_trimestre_2021) %>% 
  dplyr::bind_rows(dptos_2_trimestre_2021) %>% 
  dplyr::bind_rows(dptos_3_trimestre_2021) %>% 
  dplyr::bind_rows(dptos_4_trimestre_2021)
    



barrio_interanual <-  barrio_anual %>% 
  inner_join (barrio_trimestre_5, by="BARRIO") %>% 
  inner_join(barrio_trimestre_6, by="BARRIO") %>% 
  inner_join(barrio_trimestre_7, by="BARRIO") %>% 
  inner_join(barrio_trimestre_8, by="BARRIO") %>% 
  mutate(V4=PROMEDIO_BARRIO_T5/PROMEDIO_BARRIO_T4-1, #variaciones trimestrales
         V5=PROMEDIO_BARRIO_T6/PROMEDIO_BARRIO_T5-1,
         V6=PROMEDIO_BARRIO_T7/PROMEDIO_BARRIO_T6-1,
         V7=PROMEDIO_BARRIO_T8/PROMEDIO_BARRIO_T7-1,
         VT=PROMEDIO_BARRIO_T8/PROMEDIO_BARRIO_T1-1) #variacion interanual
#agrupar barrios repetidos

ggplot()+
  geom_bar(data=barrio_interanual, aes(y=reorder(BARRIO, -VT), weight=VT, fill=NA), alpha=.8)+
  geom_bar(data=barrio_interanual, aes(y=BARRIO, weight=VT, fill=""), color="black", alpha=.7)+

  labs(x="Variación porcentual de precios", 
       y="Barrio",
       title="Variación porcentual de precios de las propiedades inmobiliarias",
       subtitle="Período 2020-2021",
       fill="Referencia")+
  geom_vline (xintercept = 0, size=1)+
  geom_vline (xintercept = mean(barrio_anual$VT), linetype="dashed", size=1)+
  #scale_fill_manual(values = c("brown4","darkseagreen"))+
  theme_minimal()


ggplot()+
  geom_sf(data=barrios_CABA, aes(fill=VT))+
  scale_fill_distiller(palette = 3) +
  theme_void()


#___
#Union a la grilla


dptos <- st_join(dpto_inter_anual, grid)

dptos_grid_trimestre_1 <- dptos %>% 
  dplyr::filter(TRIMESTRE==1) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T1=n(),
            PROMEDIO_T1=mean(USD_M2),
            SD_T1=sd(USD_M2), 
            CV_T1=SD_T1/PROMEDIO_T1)

dptos_grid_trimestre_2 <- dptos %>% 
  dplyr::filter(TRIMESTRE==2) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T2=n(),
            PROMEDIO_T2=mean(USD_M2),
            SD_T2=sd(USD_M2),
            CV_T2=SD_T2/PROMEDIO_T2)

dptos_grid_trimestre_3 <- dptos %>% 
  dplyr::filter(TRIMESTRE==3) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T3=n(),
            PROMEDIO_T3=mean(USD_M2),
            SD_T3=sd(USD_M2),
            CV_T3=SD_T3/PROMEDIO_T3)

dptos_grid_trimestre_4 <- dptos %>% 
  dplyr::filter(TRIMESTRE==4) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T4=n(),
            PROMEDIO_T4=mean(USD_M2),
            SD_T4=sd(USD_M2),
            CV_T4=SD_T4/PROMEDIO_T4)

dptos_grid_trimestre_5 <- dptos %>% 
  dplyr::filter(TRIMESTRE==5) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T5=n(),
            PROMEDIO_T5=mean(USD_M2),
            SD_T5=sd(USD_M2), 
            CV_T5=SD_T5/PROMEDIO_T5)

dptos_grid_trimestre_6 <- dptos %>% 
  dplyr::filter(TRIMESTRE==6) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T6=n(),
            PROMEDIO_T6=mean(USD_M2),
            SD_T6=sd(USD_M2),
            CV_T6=SD_T6/PROMEDIO_T6)

dptos_grid_trimestre_7 <- dptos %>% 
  dplyr::filter(TRIMESTRE==7) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T7=n(),
            PROMEDIO_T7=mean(USD_M2),
            SD_T7=sd(USD_M2),
            CV_T7=SD_T7/PROMEDIO_T7)

dptos_grid_trimestre_8 <- dptos %>% 
  dplyr::filter(TRIMESTRE==8) %>% 
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(CANTIDAD_T8=n(),
            PROMEDIO_T8=mean(USD_M2),
            SD_T8=sd(USD_M2),
            CV_T8=SD_T8/PROMEDIO_T8)

grid_anual <- grid %>% 
  left_join(dptos_grid_trimestre_1, by="index") %>% 
  left_join(dptos_grid_trimestre_2, by="index") %>% 
  left_join(dptos_grid_trimestre_3, by="index") %>% 
  left_join(dptos_grid_trimestre_4, by="index") %>% 
  left_join(dptos_grid_trimestre_5, by="index") %>% 
  left_join(dptos_grid_trimestre_6, by="index") %>% 
  left_join(dptos_grid_trimestre_7, by="index") %>% 
  left_join(dptos_grid_trimestre_8, by="index") %>% 
  mutate(V1=PROMEDIO_T2/PROMEDIO_T1-1,
         V2=PROMEDIO_T3/PROMEDIO_T2-1,
         V3=PROMEDIO_T4/PROMEDIO_T3-1,
         V4=PROMEDIO_T5/PROMEDIO_T4-1,
         V5=PROMEDIO_T6/PROMEDIO_T5-1,
         V6=PROMEDIO_T7/PROMEDIO_T6-1,
         V7=PROMEDIO_T8/PROMEDIO_T7-1,
         VT=PROMEDIO_T8/PROMEDIO_T1-1,
         PROMEDIO_T=(PROMEDIO_T1+PROMEDIO_T2+PROMEDIO_T3+PROMEDIO_T4+
                       PROMEDIO_T5+PROMEDIO_T6+PROMEDIO_T7+PROMEDIO_T8)/8, 
         id=1:nrow(.))

grid_sd <- grid_anual %>% 
  as.data.frame() %>% 
  select(PROMEDIO_T1, PROMEDIO_T2, PROMEDIO_T3, PROMEDIO_T4,
         PROMEDIO_T5, PROMEDIO_T6, PROMEDIO_T7, PROMEDIO_T8)

row_sd1 <- apply(grid_sd, 1, sd, na.rm = TRUE) %>% as.data.frame() %>% 
  rename_with(.cols = 1, ~"SD_GENERAL")

row_sd1 <- row_sd1 %>% 
  mutate(id=1:nrow(row_sd1))
  

grid_interanual <- left_join(grid_anual, row_sd1, by="id")
grid_interanual <- grid_interanual %>% mutate(CV_T=SD_GENERAL/PROMEDIO_T)  

seleccion <- grid_interanual %>%
  dplyr::filter(CANTIDAD_T1>4) %>% 
  dplyr::filter(CANTIDAD_T2>4) %>% 
  dplyr::filter(CANTIDAD_T3>4) %>% 
  dplyr::filter(CANTIDAD_T4>4) %>% 
  dplyr::filter(CANTIDAD_T5>4) %>% 
  dplyr::filter(CANTIDAD_T6>4) %>% 
  dplyr::filter(CANTIDAD_T7>4) %>% 
  dplyr::filter(CANTIDAD_T8>4) 


mapa_CABA <- get_stamenmap(bbox = c("left" = -58.5560950647, "bottom" = -34.7154185163, "right" = -58.313297209, "top" = -34.5092633245),
                           maptype = "terrain",
                           zoom=13,
                           source = "google",
                           color="bw")


ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) + 
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(seleccion, !is.na(PROMEDIO_T)), aes(fill=PROMEDIO_T), alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Precio promedio del metro cuadrado",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       fill="Promedio \n(USD/m²)")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))


ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) +  
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(seleccion, !is.na(SD_GENERAL)), aes(fill=SD_GENERAL), alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Desvío estándar del promedio trimestral",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       fill="Devío \nestándar (USD/m²)")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))


ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) +   
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(seleccion, !is.na(CV_T)), aes(fill=CV_T), alpha=.7, color=NA, inherit.aes = FALSE)+
  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  labs(title="Coeficiente de variación del promedio trimestral",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       fill="Coeficiente \nde variación")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))



ggmap(mapa_CABA, darken = c(0.4, "white"))+
  geom_sf(data = partidos_AMBA_solido, fill="white", color=NA, inherit.aes = FALSE) + 
  geom_sf(data = partidos_AMBA, fill=NA, size=.5, color="grey50", inherit.aes = FALSE) +   
  geom_sf(data=barrios_CABA, fill=NA, color="black", size=1, inherit.aes = FALSE)+
  geom_sf(data=subset(seleccion, !is.na(VT)), aes(fill=VT), alpha=.7, color=NA, inherit.aes = FALSE)+
#  scale_fill_viridis_c(option = "magma", direction = -1, alpha=.7) +
  scale_fill_gradient2(low = "darkred", mid="white", high = "darkgreen")+
  labs(title="Variación interanual de promedios",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       fill="Promedio (%)")+
  theme_void()+
  theme(plot.title = element_text(size=22),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(size=15),
        legend.title = element_text(size=15))


# Modelo 1 = Desvio estandard - promedio
model1 <- lm(SD_GENERAL ~ PROMEDIO_T, data = seleccion)
model1
summary(model1) 

my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"

ggplot(seleccion, aes(x = PROMEDIO_T, y = SD_GENERAL)) +
  geom_point(color="grey30") +
  stat_smooth(method = lm, formula = y ~ x, fill="red", alpha=.1, color="darkred", linetype="dashed")+
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
  labs(title="Correlación variabilidad trimestral - promedio trimestral",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       x="Precio promedio propiedades en venta (USD/m²)",
       y="Desvío estándar (USD/m²)")+
  theme_minimal()


# Modelo 2 = Desvio estandard - variacion interanual
model2 <- lm(VT ~ SD_GENERAL, data = seleccion)
model2
summary(model2)

ggplot(seleccion, aes(x = SD_GENERAL, y = VT)) +
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
  labs(title="Correlación variabilidad trimestral - recuperación del precio",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo Enero 2020 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop",
       x="Desvío estándar (USD/m²)",
       y="Variación total del precio trimestral")+
  theme_minimal()


  #grid_anual %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/variacion-anual.csv")
grid_interanual %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/variacion-interanual.csv")

#grid_anual %>% as.data.frame() %>% select(-geometry) %>% write_csv("data/processed/en-proceso/variacion-anual.csv")
grid_interanual %>% as.data.frame() %>% 
  select(-geometry) %>%   
  dplyr::filter(CANTIDAD_T1>4) %>% 
  dplyr::filter(CANTIDAD_T2>4) %>% 
  dplyr::filter(CANTIDAD_T3>4) %>% 
  dplyr::filter(CANTIDAD_T4>4) %>% 
  dplyr::filter(CANTIDAD_T5>4) %>% 
  dplyr::filter(CANTIDAD_T6>4) %>% 
  dplyr::filter(CANTIDAD_T7>4) %>% 
  dplyr::filter(CANTIDAD_T8>4) %>%  
  write_csv("data/processed/en-proceso/variacion-interanual-clean.csv")

mean(grid_interanual$PROMEDIO_T, na.rm = TRUE)

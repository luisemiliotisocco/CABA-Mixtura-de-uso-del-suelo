library(sf)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)    
library(matrixStats)
#install.packages("ggpmisc")
library(ggpmisc)
library(ggmap)
library(nngeo)
library(writexl)

proj <- 4326
sf::sf_use_s2(FALSE) #apagamos la geometría esférica
my.formula <- y ~ x
myformat <- "y = %s + %s x      |      R²: %s"
myformat2 <- "y = %s %s x      |      R²: %s"

#___

manzanas <- st_read("data/raw/manzanas/manzanas.shp") %>% 
  st_transform(proj) %>% 
  rename(index=OBJECTID) %>% 
  select(index)

 
manzanas_uso <- read.csv("data/processed/manzanas/manzanas-con-usos.csv", stringsAsFactors = TRUE,
                         encoding = "UTF-8") %>% 
  mutate(index=as.numeric(index)) %>% 
  mutate(PERIMETRO_AREA=SUMA_PERIMETRO/AREA)
manzanas_uso$PERIMETRO_AREA[is.infinite(manzanas_uso$PERIMETRO_AREA)] <- 0


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
  st_join(manzanas) 


### P1: PREPANDEMICOS

dptos_1_trimestre <- dptos_1_semestre %>% 
  dplyr::filter(AÑO==2020 & TRIMESTRE==1) %>% # filtro hasta marzo 2020
  st_difference()
# 14082

# Limpieza de valores extremos
Q1=quantile(dptos_1_trimestre$USD_M2, c(.25))
Q3=quantile(dptos_1_trimestre$USD_M2, c(.75))
IQR= Q3-Q1

lim_inf=Q1-1.5*IQR
lim_sup=Q3+1.5*IQR  

dptos_1_trimestre <- filter(dptos_1_trimestre, USD_M2 >lim_inf & USD_M2 < lim_sup)
# 13595

# Dptos que no se unierorn espacialmente, por no coincidor con la manzana:
# calculo cual es la manzana mas cercana y se la uno al index 
vacios <- dptos_1_trimestre %>% 
  filter(is.na(index))

# Unimos las 3880 propiedades que no coinciden con una manzana a la manzana mas cercana
cercanos <- st_nearest_feature(vacios, manzanas)

vacios$index <- cercanos

dptos_1_trimestre <- dptos_1_trimestre %>% filter(!is.na(index)) %>% 
  bind_rows(vacios)

# Promedio por manzana
dptos_1_trimestre_df  <- dptos_1_trimestre %>%  
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(USD_M2=mean(USD_M2))

manzanas_1_T <- manzanas %>% 
  left_join(manzanas_uso, by='index') %>% 
  left_join(dptos_1_trimestre_df, by='index') %>% 
  filter(!is.na(USD_M2)) %>% 
  select(-X)

ggplot(manzanas_1_T)+
  geom_sf(aes(fill=USD_M2), color=NA)+
  scale_fill_viridis_c(option = 'magma', direction = -1)

manzanas_1_T %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  write.csv('data/processed/mercado-inmobiliario/MI_1T.csv')


# Regresiones
ggplot(manzanas_1_T, aes(x = PERIMETRO_AREA , y = USD_M2)) +
  geom_point(color="grey20", size=.02, alpha=.7) +
  stat_density2d_filled(aes(fill=..level..,alpha=..level..), show.legend = FALSE) + 
  stat_density_2d(color='grey10', linetype='dashed', size=.5, show.legend = FALSE)+
  stat_smooth(method = lm, fill="red", alpha=.3, color="darkred", linetype="dashed")+
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
  labs(title="Correlación granulado de usos - promedio USD/m²",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo: hasta Marzo 2020",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Interacción de usos (perímetro/ha)",
       y="Precio promedio propiedades en venta (USD/m²)")+
  scale_fill_viridis_d(option = 'inferno', direction = -1)+
  scale_alpha_discrete(range = c(0, 0.7))+
  theme_minimal()



model <- lm(USD_M2 ~ ENT + INDICE_SIMPSON + PERIMETRO_AREA + DENS_POB, data = manzanas_1_T)
model
summary(model)


model2 <- lm(USD_M2 ~ AREA_R + AREA_C + AREA_S + AREA_P + 
               AREA_EP + AREA_RC, data = manzanas_1_T)
model2
summary(model2)



### P2: Marzo 2020 - Diciembre 2020

# T2
dptos_2_trimestre <- dptos_1_semestre %>% 
  dplyr::filter(AÑO==2020 & TRIMESTRE==2) %>% # filtro hasta marzo 2020
  st_difference()
# 8563 dptos


# T3
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
  st_difference()
# 2541 dptos


# T4
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
  st_difference()
# 2182 dptos


dptos_2P <- dptos_3_trimestre %>% 
  bind_rows(dptos_4_trimestre) %>% 
  st_join(manzanas) %>% 
  bind_rows(dptos_2_trimestre)
# 13286

# Limpieza de valores extremos
Q1=quantile(dptos_2P$USD_M2, c(.25))
Q3=quantile(dptos_2P$USD_M2, c(.75))
IQR= Q3-Q1

lim_inf=Q1-1.5*IQR
lim_sup=Q3+1.5*IQR  

dptos_2P <- filter(dptos_2P, USD_M2 >lim_inf & USD_M2 < lim_sup)
# 12802

# Dptos que no se unierorn espacialmente, por no coincidor con la manzana:
# calculo cual es la manzana mas cercana y se la uno al index 
vacios <- dptos_2P %>% 
  filter(is.na(index))

# Unimos las 3880 propiedades que no coinciden con una manzana a la manzana mas cercana
cercanos <- st_nearest_feature(vacios, manzanas)
vacios$index <- cercanos

dptos_2P <- dptos_2P %>% filter(!is.na(index)) %>% 
  bind_rows(vacios)

# Promedio por manzana
dptos_2P_df  <- dptos_2P %>%  
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(USD_M2=mean(USD_M2))

manzanas_2P <- manzanas %>% 
  left_join(manzanas_uso, by='index') %>% 
  left_join(dptos_2P_df, by='index') %>% 
  filter(!is.na(USD_M2)) %>% 
  select(-X)

ggplot(manzanas_2P)+
  geom_sf(aes(fill=USD_M2), color=NA)+
  scale_fill_viridis_c(option = 'magma', direction = -1)

manzanas_2P %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  write.csv('data/processed/mercado-inmobiliario/MI_2P.csv')


# Regresiones
ggplot(manzanas_2P, aes(x = ENT , y = USD_M2)) +
  geom_point(color="grey20", size=.02, alpha=.7) +
  stat_density2d_filled(aes(fill=..level..,alpha=..level..), show.legend = FALSE) + 
  stat_density_2d(color='grey10', linetype='dashed', size=.5, show.legend = FALSE)+
  stat_smooth(method = lm, fill="red", alpha=.3, color="darkred", linetype="dashed")+
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
  labs(title="Correlación entropía de usos - promedio USD/m²",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo: Marzo 2020 - Diciembre2020",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Índice de entropía",
       y="Precio promedio propiedades en venta (USD/m²)")+
  scale_fill_viridis_d(option = 'inferno', direction = -1)+
  scale_alpha_discrete(range = c(0, 0.7))+
  theme_minimal()



model3 <- lm(USD_M2 ~ ENT + INDICE_SIMPSON + PERIMETRO_AREA + DENS_POB, data = manzanas_2P)
model3
summary(model3)


model4 <- lm(USD_M2 ~ AREA_R + AREA_C + AREA_S + AREA_P + 
               AREA_EP + AREA_RC, data = manzanas_2P)
model4
summary(model4)





### P3: Enero 2021 - Diciembre 2021

# T5
dptos_5_trimestre <- st_read("data/raw/propiedades/2021/Venta/1-Trimestre/deptos_vta_1°trimestre_2021_v2.shp", 
                                  stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=5) %>% 
  st_difference()
# 1851 dptos


# T6
dptos_6_trimestre <- st_read("data/raw/propiedades/2021/Venta/2-Trimestre/deptoventa_2trimestre_2021.shp", 
                             stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=6) %>% 
  st_difference()
# 2240 dptos


# T7
dptos_7_trimestre <- st_read("data/raw/propiedades/2021/Venta/3-Trimestre/Deptos_Vta_3°Trimestre_2021.shp", 
                             stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=7) %>% 
  st_difference()
# 2403 dptos


# T8
dptos_8_trimestre <- st_read("data/raw/propiedades/2021/Venta/4-Trimestre/deptos_vta_4°trimestre_2021.shp", 
                             stringsAsFactors = TRUE, options = "ENCODING=UTF8") %>% 
  st_transform(proj) %>% 
  select(U_S_M2, ANTIG_EDAD, AMBIENTES) %>% 
  rename(USD_M2=U_S_M2,
         ANTIG=ANTIG_EDAD) %>% 
  mutate(TRIMESTRE=8) %>% 
  st_difference()
# 2347 dptos



dptos_3P <- dptos_5_trimestre %>% 
  bind_rows(dptos_6_trimestre) %>% 
  bind_rows(dptos_7_trimestre) %>% 
  bind_rows(dptos_7_trimestre) %>% 
  st_join(manzanas)
# 8897

# Limpieza de valores extremos
Q1=quantile(dptos_3P$USD_M2, c(.25))
Q3=quantile(dptos_3P$USD_M2, c(.75))
IQR= Q3-Q1

lim_inf=Q1-1.5*IQR
lim_sup=Q3+1.5*IQR  

dptos_3P <- filter(dptos_3P, USD_M2 >lim_inf & USD_M2 < lim_sup)
# 8445 

# Dptos que no se unierorn espacialmente, por no coincidor con la manzana:
# calculo cual es la manzana mas cercana y se la uno al index 
vacios <- dptos_3P %>% 
  filter(is.na(index))

# Unimos las 3880 propiedades que no coinciden con una manzana a la manzana mas cercana
cercanos <- st_nearest_feature(vacios, manzanas)
vacios$index <- cercanos

dptos_3P <- dptos_3P %>% filter(!is.na(index)) %>% 
  bind_rows(vacios)

# Promedio por manzana
dptos_3P_df  <- dptos_3P %>%  
  as.data.frame() %>% 
  group_by(index) %>% 
  summarise(USD_M2=mean(USD_M2))

manzanas_3P <- manzanas %>% 
  left_join(manzanas_uso, by='index') %>% 
  left_join(dptos_3P_df, by='index') %>% 
  filter(!is.na(USD_M2)) %>% 
  select(-X)

ggplot(manzanas_3P)+
  geom_sf(aes(fill=USD_M2), color=NA)+
  scale_fill_viridis_c(option = 'magma', direction = -1)

manzanas_3P %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  write.csv('data/processed/mercado-inmobiliario/MI_3P.csv')



# Regresiones
ggplot(manzanas_3P, aes(x = INDICE_SIMPSON , y = USD_M2)) +
  geom_point(color="grey20", size=.02, alpha=.7) +
  stat_density2d_filled(aes(fill=..level..,alpha=..level..), show.legend = FALSE) + 
  stat_density_2d(color='grey10', linetype='dashed', size=.5, show.legend = FALSE)+
  stat_smooth(method = lm, fill="red", alpha=.3, color="darkred", linetype="dashed")+
  stat_poly_eq(
    formula = my.formula, output.type = "numeric",
    mapping = aes(
      label = 
        sprintf(
          myformat2,
          format(stat(coef.ls)[[1]][[1, "Estimate"]], scientific = FALSE, digits =4),
          format(stat(coef.ls)[[1]][[2, "Estimate"]], scientific = FALSE, digits =4),
          formatC(stat(r.squared)))),
    vstep = 0.1)+
  labs(title="Correlación diversidad de usos - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires \nPeríodo: Enero 2021 - Diciembre 2021",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Índice de diversidad (Simpson)",
       y="Precio promedio propiedades en venta (USD/m²)")+
  scale_fill_viridis_d(option = 'inferno', direction = -1)+
  scale_alpha_discrete(range = c(0, 0.7))+
  xlim(c(0.73,1))+
  ylim(c(0,5000))+
  theme_minimal()

#hasta Marzo 2020
#Marzo 2020 - Diciembre 2020
#Enero 2021 - Diciembre 2021

model5 <- lm(USD_M2 ~ ENT + INDICE_SIMPSON + PERIMETRO_AREA + DENS_POB, data = manzanas_3P)
model5
summary(model5)


model6 <- lm(USD_M2 ~ AREA_R + AREA_C + AREA_S + AREA_P + 
               AREA_EP + AREA_RC, data = manzanas_3P)
model6
summary(model6)




#####

# ENTROPIA
ggplot() +
  stat_smooth(data=manzanas_1_T, aes(x = PERIMETRO_AREA , y = USD_M2, color='Prepandémico'), fill=NA)+
  stat_smooth(data=manzanas_2P, aes(x = PERIMETRO_AREA , y = USD_M2, color='Marzo 2020 - Diciembre 2020'), fill=NA)+
  stat_smooth(data=manzanas_3P, aes(x = PERIMETRO_AREA , y = USD_M2, color='Enero 2021 - Diciembre 2021'), fill=NA)+
  #ylim(c(2000,2300))+
  #xlim(c(0.5,1))+
  labs(title="Correlación granulado de usos - valor promedio del m² residencial",
       subtitle = "Propiedades en venta en la Ciudad Autónoma de Buenos Aires",
       caption="Elaboración propia con datos de Argenprop y de BAdata",
       x="Interacción de usos (m perímetro interacción/ha)",
       y="Precio promedio propiedades en venta (USD/m²)",
       color='Período')+  
  scale_color_manual(values=c('Prepandémico'='#f78a2f',
                              'Marzo 2020 - Diciembre 2020'='#c13262',
                              'Enero 2021 - Diciembre 2021'='#370065'))+
  theme_minimal()


  
  geom_point(color="grey20", size=.02, alpha=.7) +
  stat_density2d_filled(aes(fill=..level..,alpha=..level..), show.legend = FALSE) + 
  stat_density_2d(color='grey10', linetype='dashed', size=.5, show.legend = FALSE)+
  stat_smooth(method = lm, fill="red", alpha=.3, color="darkred", linetype="dashed")+



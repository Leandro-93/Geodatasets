library(dplyr)
library(sf)

nactot = read.csv("FBB 2024/Data Lab/Natalidad_serie/nactot 2010-2015-2018.csv", sep= ";")

nactot = nactot %>% rename(Provincia = provres, Departamento = depres, Peso_al_nacer = pesonac, Total = cant)

nactot_clean = nactot %>% filter(!Provincia %in% c('98', '99')& !Departamento %in% c('999')) 

# Ajustar los valores de Provincia y departamento
nactot_clean$Provincia = ifelse(nchar(nactot_clean$Provincia) < 2, paste0("0", nactot_clean$Provincia),
                                  nactot_clean$Provincia)

nactot_clean$Departamento <- ifelse(nchar(nactot_clean$Departamento) < 2, paste0("00", nactot_clean$Departamento),
                                     ifelse(nchar(nactot_clean$Departamento) == 2, paste0("0", nactot_clean$Departamento),
                                            nactot_clean$Departamento))

nactot_clean$CODGEO = paste0(nactot_clean$Provincia, nactot_clean$Departamento)

#Mergeamos todos los puntos sobre un poligono de deptos 
deptos<-st_read("C:/Users/leand/Desktop/FBB 2024/Data Lab/Datos_M_N_2022/Codgeo_Pais_x_dpto_con_datos/pxdptodatosok.shp") %>% rename(CODGEO = link)

join_CODGEO <- merge(nactot_clean, deptos, by ="CODGEO", all.x = TRUE)

join_CODGEO_sf = join_CODGEO %>% select(ano, CODGEO, provincia, departamen, Peso_al_nacer, personas, hogares, viv_part, Total) 

write.csv(join_CODGEO,"C:/Users/leand/Desktop/FBB 2024/Data Lab/Natalidad_serie/nactot_deptos.csv")





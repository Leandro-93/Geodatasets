#plot valores
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)

Alq_base <- read_excel("Alquileres temporarios.xlsx")
View(Alq_base)

#limpieza base - filtro por coordenadas y existencia de superficie y valores de operacion
sf_Alq_base <- Alq_base %>% 
  filter(!is.na(Latitud) & !is.na(Longitud))

Alq_f_1 <- sf_Alq_base %>% filter(MontoOperacion > 1 & PropiedadSuperficieTotal > 1)

#limpieza por ubicacion
Barrios_BADATA <- st_read("C:\\Users\\leand\\Desktop\\GCBA DGAUR\\AirBnb\\190423_Barrios_BA Data\\190423_Barrios_BA Data")

st_crs(Barrios_BADATA)

#creo un objeto espacial
Alq_f_1_4326 <- st_as_sf(Alq_f_1, coords = c("Longitud", "Latitud"),
                         crs= 4326)

#igualo coordenadas
Alq_f_1_4326_9001 <- Alq_f_1_4326 %>% st_transform(st_crs(Barrios_BADATA))

st_crs(Alq_f_1_4326_9001)


#interseccion
Alq_Barrios_BA <- st_intersection(Alq_f_1_4326_9001, Barrios_BADATA) #7385 total

#mapeo
base_plot <- ggplot() +
  theme_void()

base_plot +
  geom_sf(data = Barrios_BADATA, color = "blue") +
  geom_sf(data = Alq_Barrios_BA, fill = "black", alpha = 0.3)

#son 7385 registros en CABA

#limpieza por valores minimos y conversion a ARS
#Consideramos como minimo 18 m2 de superficie para un depto y un precio de 15000 pesos para un alquiler (Informe CEM 2019)

valor_multiplicacion <- 195.08
condicion <- Alq_Barrios_BA$Nombre == "Dolares"

Alq_Barrios_BA <- mutate(Alq_Barrios_BA, ConversionARS = ifelse(condicion, MontoOperacion * valor_multiplicacion, MontoOperacion))

Alq_Barrios_BA <- mutate(Alq_Barrios_BA, ConversionARS = as.integer(ConversionARS))

st_write(Alq_Barrios_BA, "C:\\Users\\leand\\Desktop\\GCBA DGAUR\\AirBnb\\Airbnb_CABA_total_ARS.shp")
write.csv(Alq_Barrios_BA, "C:\\Users\\leand\\Desktop\\GCBA DGAUR\\AirBnb\\Airbnb_CABA_total_ARS.csv")
writexl::write_xlsx(Alq_Barrios_BA, "C:\\Users\\leand\\Desktop\\GCBA DGAUR\\AirBnb\\Airbnb_CABA_total_ARS.xlsx")


